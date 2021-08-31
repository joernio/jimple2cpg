package io.joern.jimple2cpg

import io.joern.jimple2cpg.passes.AstCreationPass
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.IntervalKeyPool
import io.shiftleft.semanticcpg.passes.cfgdominator.CfgDominatorPass
import io.shiftleft.semanticcpg.passes.codepencegraph.CdgPass
import io.shiftleft.semanticcpg.passes.containsedges.ContainsEdgePass
import io.shiftleft.semanticcpg.passes.languagespecific.fuzzyc.MethodStubCreator
import io.shiftleft.semanticcpg.passes.linking.calllinker.StaticCallLinker
import io.shiftleft.semanticcpg.passes.linking.linker.Linker
import io.shiftleft.semanticcpg.passes.metadata.MetaDataPass
import io.shiftleft.semanticcpg.passes.methoddecorations.MethodDecoratorPass
import io.shiftleft.semanticcpg.passes.namespacecreator.NamespaceCreator
import io.shiftleft.semanticcpg.passes.typenodes.{TypeDeclStubCreator, TypeNodePass}
import io.shiftleft.semanticcpg.passes.{CfgCreationPass, FileCreationPass}
import io.shiftleft.x2cpg.SourceFiles
import io.shiftleft.x2cpg.X2Cpg.newEmptyCpg
import org.slf4j.LoggerFactory
import soot.options.Options
import soot.{G, PhaseOptions, Scene}

import java.io.{File => JFile}
import java.nio.file.Files
import java.util.zip.ZipFile
import scala.jdk.CollectionConverters.EnumerationHasAsScala
import scala.language.postfixOps
import scala.util.Using

object Jimple2Cpg {
  val language = "JIMPLEPARSER"
}

class Jimple2Cpg {

  import Jimple2Cpg._

  private val logger = LoggerFactory.getLogger(classOf[Jimple2Cpg])

  /** Creates a CPG from Jimple.
    *
    * @param sourceCodePath The path to the Jimple code or code that can be transformed into Jimple.
    * @param outputPath     The path to store the CPG. If `outputPath` is `None`, the CPG is created in-memory.
    * @return The constructed CPG.
    */
  def createCpg(
      sourceCodePath: String,
      outputPath: Option[String] = None
  ): Cpg = {
    configureSoot(sourceCodePath)
    val cpg = newEmptyCpg(outputPath)

    val metaDataKeyPool = new IntervalKeyPool(1, 100)
    val typesKeyPool    = new IntervalKeyPool(100, 1000100)
    val methodKeyPool   = new IntervalKeyPool(first = 1000100, last = Long.MaxValue)

    new MetaDataPass(cpg, language, Some(metaDataKeyPool)).createAndApply()

    val sourceFileExtensions = Set(".class", ".jimple")
    val zipFileExtensions    = Set(".jar", ".war")
    // Unpack any archives on the path onto the source code path as project root
    val archives = SourceFiles.determine(Set(sourceCodePath), zipFileExtensions)
    archives.map(new ZipFile(_)).foreach(unzipArchive(_, sourceCodePath))

    val sourceFileNames = SourceFiles.determine(Set(sourceCodePath), sourceFileExtensions)
    val astCreator      = new AstCreationPass(sourceCodePath, sourceFileNames, cpg, methodKeyPool)
    astCreator.createAndApply()

    new CfgCreationPass(cpg).createAndApply()

    new NamespaceCreator(cpg).createAndApply()
    new FileCreationPass(cpg).createAndApply()

    new TypeNodePass(astCreator.global.usedTypes.keys().asScala.toList, cpg, Some(typesKeyPool))
      .createAndApply()
    new TypeDeclStubCreator(cpg).createAndApply()
    new MethodStubCreator(cpg).createAndApply()
    new MethodDecoratorPass(cpg).createAndApply()

    new ContainsEdgePass(cpg).createAndApply()
    new Linker(cpg).createAndApply()
    new StaticCallLinker(cpg).createAndApply()

    new CfgDominatorPass(cpg).createAndApply()
    new CdgPass(cpg).createAndApply()

    closeSoot()

    cpg
  }

  private def configureSoot(sourceCodePath: String): Unit = {
    // set application mode
    Options.v().set_app(false)
    Options.v().set_whole_program(false)
    // make sure classpath is configured correctly
    Options.v().set_soot_classpath(sourceCodePath)
    Options.v().set_prepend_classpath(true)
    // keep debugging info
    Options.v().set_keep_line_number(true)
    Options.v().set_keep_offset(true)
    // ignore library code
    Options.v().set_no_bodies_for_excluded(true)
    Options.v().set_allow_phantom_refs(true)
    // keep variable names
    PhaseOptions.v().setPhaseOption("jb", "use-original-names:true")
    Scene.v().loadBasicClasses()
    Scene.v().loadDynamicClasses()
  }

  private def closeSoot(): Unit = {
    G.reset()
  }

  /** Unzips a ZIP file into a sequence of files. All files unpacked are deleted at the end of CPG construction.
    *
    * @param zf The ZIP file to extract.
    * @param sourceCodePath The project root path to unpack to.
    */
  private def unzipArchive(zf: ZipFile, sourceCodePath: String) = scala.util.Try {
    Using.resource(zf) { zip: ZipFile =>
      // Copy zipped files across
      zip
        .entries()
        .asScala
        .filter(!_.isDirectory)
        .filter(_.getName.contains(".class"))
        .flatMap(entry => {
          val destFile = new JFile(sourceCodePath + JFile.separator + entry.getName)
          val dirName = destFile.getAbsolutePath
            .substring(0, destFile.getAbsolutePath.lastIndexOf(JFile.separator))
          // Create directory path
          new JFile(dirName).mkdirs()
          try {
            if (destFile.exists()) destFile.delete()
            Using.resource(zip.getInputStream(entry)) { input =>
              Files.copy(input, destFile.toPath)
            }
            destFile.deleteOnExit()
            Option(destFile)
          } catch {
            case e: Exception =>
              logger.warn(
                s"Encountered an error while extracting entry ${entry.getName} from archive ${zip.getName}.",
                e
              )
              Option.empty
          }
        })
        .toSeq
    }
  }

}
