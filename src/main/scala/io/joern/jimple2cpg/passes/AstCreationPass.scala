package io.joern.jimple2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}
import org.slf4j.LoggerFactory
import soot.Scene

import java.util.concurrent.ConcurrentHashMap
import scala.tools.nsc

case class Global(
    usedTypes: ConcurrentHashMap[String, Boolean] = new ConcurrentHashMap[String, Boolean]()
)

class AstCreationPass(codeDir: String, filenames: List[String], cpg: Cpg, keyPool: IntervalKeyPool)
    extends ParallelCpgPass[String](cpg, keyPools = Some(keyPool.split(filenames.size))) {

  val global: Global = Global()
  private val logger = LoggerFactory.getLogger(classOf[AstCreationPass])

  override def partIterator: Iterator[String] = filenames.iterator

  override def runOnPart(filename: String): Iterator[DiffGraph] = {
    val qualifiedClassName = getQualifiedClassPath(filename)
    try {
      new AstCreator(filename, global)
        .createAst(Scene.v().loadClassAndSupport(qualifiedClassName))
    } catch {
      case e: Exception =>
        logger.warn("Cannot parse: " + filename, e)
        Iterator()
    }
  }

  /** Formats the file name the way Soot refers to classes within a class path. e.g.
    * /unrelated/paths/class/path/Foo.class => class.path.Foo
    *
    * @param filename the file name to transform.
    * @return the correctly formatted class path.
    */
  def getQualifiedClassPath(filename: String): String = {
    filename
      .replace(codeDir + nsc.io.File.separator, "")
      .replace(nsc.io.File.separator, ".")
      .replace(".class", "")
  }

}
