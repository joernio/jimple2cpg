package io.joern.jimple2cpg

import io.joern.jimple2cpg.passes.AstCreationPass
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.IntervalKeyPool
import io.shiftleft.semanticcpg.passes.cfgdominator.CfgDominatorPass
import io.shiftleft.semanticcpg.passes.codepencegraph.CdgPass
import io.shiftleft.semanticcpg.passes.{CfgCreationPass, FileCreationPass}
import io.shiftleft.semanticcpg.passes.containsedges.ContainsEdgePass
import io.shiftleft.semanticcpg.passes.languagespecific.fuzzyc.MethodStubCreator
import io.shiftleft.semanticcpg.passes.linking.calllinker.StaticCallLinker
import io.shiftleft.semanticcpg.passes.linking.linker.Linker
import io.shiftleft.semanticcpg.passes.metadata.MetaDataPass
import io.shiftleft.semanticcpg.passes.methoddecorations.MethodDecoratorPass
import io.shiftleft.semanticcpg.passes.namespacecreator.NamespaceCreator
import io.shiftleft.semanticcpg.passes.typenodes.{TypeDeclStubCreator, TypeNodePass}
import io.shiftleft.x2cpg.SourceFiles
import io.shiftleft.x2cpg.X2Cpg.newEmptyCpg
import soot.{PhaseOptions, Scene}
import soot.options.Options

import scala.jdk.CollectionConverters.EnumerationHasAsScala

object Jimple2Cpg {
  val language = "JIMPLEPARSER"
}

class Jimple2Cpg {

  import Jimple2Cpg._

  /** Creates a CPG from Jimple.
    *
    * @param sourceCodePath The path to the Jimple code or code that can be transformed into Jimple.
    * @param outputPath The path to store the CPG. If `outputPath` is `None`, the CPG is created in-memory.
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
    val sourceFileNames      = SourceFiles.determine(Set(sourceCodePath), sourceFileExtensions)
    val astCreator           = new AstCreationPass(sourceCodePath, sourceFileNames, cpg, methodKeyPool)
    astCreator.createAndApply()

    new CfgCreationPass(cpg).createAndApply()

    new NamespaceCreator(cpg).createAndApply()
    new FileCreationPass(cpg).createAndApply()

    new TypeNodePass(astCreator.global.usedTypes.keys().asScala.toList, cpg, Some(typesKeyPool))
      .createAndApply()
    new TypeDeclStubCreator(cpg).createAndApply()

    new ContainsEdgePass(cpg).createAndApply()
    new Linker(cpg).createAndApply()

    new MethodStubCreator(cpg).createAndApply()
    new MethodDecoratorPass(cpg).createAndApply()
    new StaticCallLinker(cpg).createAndApply()

    new CfgDominatorPass(cpg).createAndApply()
    new CdgPass(cpg).createAndApply()
    cpg
  }

  def configureSoot(sourceCodePath: String): Unit = {
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

}
