package io.joern.jimple2cpg.passes

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{NewNamespaceBlock, NewTypeDecl}
import io.shiftleft.passes.DiffGraph
import io.shiftleft.x2cpg.Ast
import soot.{RefType, SootClass}

import java.io.File

class AstCreator(filename: String, global: Global) {

  val diffGraph: DiffGraph.Builder = DiffGraph.newBuilder

  /** Add `typeName` to a global map and return it. The
    * map is later passed to a pass that creates TYPE
    * nodes for each key in the map.
    */
  private def registerType(typeName: String): String = {
    global.usedTypes.put(typeName, true)
    typeName
  }

  /** Entry point of AST creation. Translates a compilation
    * unit created by JavaParser into a DiffGraph containing
    * the corresponding CPG AST.
    */
  def createAst(cls: SootClass): Iterator[DiffGraph] = {
    val astRoot = astForCompilationUnit(cls)
    storeInDiffGraph(astRoot)
    Iterator(diffGraph.build)
  }

  /** Copy nodes/edges of given `AST` into the diff graph
    */
  private def storeInDiffGraph(ast: Ast): Unit = {
    ast.nodes.foreach { node =>
      diffGraph.addNode(node)
    }
    ast.edges.foreach { edge =>
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.AST)
    }
    ast.conditionEdges.foreach { edge =>
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.CONDITION)
    }
    ast.argEdges.foreach { edge =>
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.ARGUMENT)
    }
  }

  /** Translate compilation unit into AST
    */
  private def astForCompilationUnit(cls: SootClass): Ast = {
    val ast = astForPackageDeclaration(cls.getPackageName)
    val namespaceBlockFullName =
      ast.root.collect { case x: NewNamespaceBlock => x.fullName }.getOrElse("none")
    ast.withChild(astForTypeDecl(cls.getType, 1, namespaceBlockFullName))
  }

  /** Translate package declaration into AST consisting of
    * a corresponding namespace block.
    */
  private def astForPackageDeclaration(packageDecl: String): Ast = {
    val absolutePath = new java.io.File(filename).toPath.toAbsolutePath.normalize().toString
    val name         = packageDecl.split("\\.").lastOption.getOrElse("")
    val namespaceBlock = NewNamespaceBlock()
      .name(name)
      .fullName(packageDecl)
    Ast(namespaceBlock.filename(absolutePath).order(1))
  }

  /** Creates the AST root for type declarations and acts as the entry point for method generation.
    */
  private def astForTypeDecl(
      typ: RefType,
      order: Int,
      namespaceBlockFullName: String
  ): Ast = {
    val fullName = typ.toQuotedString
    val filename =
      if (fullName.contains('.'))
        s"${File.separator}${fullName.replace(".", File.separator).replace("[]", "")}.class"
      else fullName
    val shortName =
      if (fullName.contains('.')) fullName.substring(fullName.lastIndexOf('.') + 1)
      else fullName

    val typeDecl = NewTypeDecl()
      .name(shortName)
      .fullName(registerType(fullName))
      .order(order)
      .filename(filename)
      .code(fullName)
      .astParentType("NAMESPACE_BLOCK")
      .astParentFullName(namespaceBlockFullName)

    // TODO: Break into methods

    Ast(typeDecl)
  }
}
