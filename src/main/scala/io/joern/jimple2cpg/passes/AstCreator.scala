package io.joern.jimple2cpg.passes

import io.shiftleft.passes.DiffGraph
import soot.SootClass

class AstCreator(filename: String, global: Global) {

  val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder

  def createAst(cls: SootClass): Iterator[DiffGraph] = {
    // TODO: Build something
    Iterator(diffGraph.build)
  }

}
