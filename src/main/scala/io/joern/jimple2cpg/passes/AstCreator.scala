package io.joern.jimple2cpg.passes

import io.shiftleft.codepropertygraph.generated.{EdgeTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewBlock,
  NewCall,
  NewMethod,
  NewMethodParameterIn,
  NewMethodReturn,
  NewNamespaceBlock,
  NewTypeDecl
}
import io.shiftleft.passes.DiffGraph
import io.shiftleft.x2cpg.Ast
import org.slf4j.LoggerFactory
import soot.jimple.{
  AssignStmt,
  BinopExpr,
  CaughtExceptionRef,
  Constant,
  GotoStmt,
  IdentityRef,
  IdentityStmt,
  IfStmt,
  InstanceFieldRef,
  InvokeExpr,
  InvokeStmt,
  LookupSwitchStmt,
  MonitorStmt,
  NewArrayExpr,
  NewExpr,
  ReturnStmt,
  ReturnVoidStmt,
  StaticFieldRef,
  Stmt,
  TableSwitchStmt,
  ThrowStmt
}
import soot.tagkit.{AbstractHost, Host}
import soot.{Body, Local, RefType, SootClass, SootMethod}

import java.io.File
import scala.jdk.CollectionConverters.CollectionHasAsScala

class AstCreator(filename: String, global: Global) {

  import AstCreator._

  private val logger               = LoggerFactory.getLogger(classOf[AstCreationPass])
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

    val methodAsts = withOrder(
      typ.getSootClass.getMethods.asScala.toList.sortWith((x, y) => x.getName > y.getName)
    ) { (m, order) =>
      astForMethod(m, typ, order)
    }

    // TODO: Fields as Member nodes

    Ast(typeDecl)
      .withChildren(methodAsts)
  }

  private def astForMethod(
      methodDeclaration: SootMethod,
      typeDecl: RefType,
      childNum: Int
  ): Ast = {
    val methodNode = createMethodNode(methodDeclaration, typeDecl, childNum)
    val lastOrder  = 2 + methodDeclaration.getParameterCount
    val methodRoot = Ast(methodNode).withChild(astForMethodReturn(methodDeclaration))

    try {
      val methodBody = methodDeclaration.retrieveActiveBody()
      val parameterAsts = withOrder(methodBody.getParameterLocals) { (p, order) =>
        astForParameter(p, order)
      }
      methodRoot
        .withChildren(parameterAsts)
        .withChild(astForMethodBody(methodBody, lastOrder))
    } catch {
      case e: RuntimeException => logger.warn("Unable to parse method body.", e)
    }

    methodRoot
  }

  private def astForParameter(parameter: Local, childNum: Int): Ast = {
    val typeFullName = registerType(parameter.getType.toQuotedString)
    val parameterNode = NewMethodParameterIn()
      .name(parameter.getName)
      .code(parameter.toString)
      .typeFullName(typeFullName)
      .order(childNum)
      .lineNumber(-1)
      .columnNumber(-1)
    Ast(parameterNode)
  }

  private def astForMethodBody(body: Body, order: Int): Ast = {
    val block = NewBlock(order = order, lineNumber = line(body), columnNumber = column(body))
    Ast(block).withChildren(
      withOrder(body.getUnits.asScala) { (x, order) =>
        astsForStatement(x, order)
      }.flatten
    )
  }

  private def astsForStatement(statement: soot.Unit, order: Int): Seq[Ast] = {
    statement match {
      case x: AssignStmt       => Seq(astForAssign(x, order))
      case x: IfStmt           => Seq()
      case x: GotoStmt         => Seq()
      case x: IdentityStmt     => Seq()
      case x: LookupSwitchStmt => Seq()
      case x: TableSwitchStmt  => Seq()
      case x: InvokeStmt       => Seq()
      case x: ReturnStmt       => Seq()
      case x: ReturnVoidStmt   => Seq()
      case x: ThrowStmt        => Seq()
      case x: MonitorStmt      => Seq()
      case x =>
        logger.warn(s"Unhandled soot.Unit type ${x.getClass}")
        Seq()
    }
  }

  private def astForValue(value: soot.Value, order: Int): Ast = {
    value match {
      case x: BinopExpr          => Ast()
      case x: Local              => Ast()
      case x: IdentityRef        => Ast()
      case x: Constant           => Ast()
      case x: InvokeExpr         => Ast()
      case x: StaticFieldRef     => Ast()
      case x: NewExpr            => Ast()
      case x: NewArrayExpr       => Ast()
      case x: CaughtExceptionRef => Ast()
      case x: InstanceFieldRef   => Ast()
      case x =>
        logger.warn(s"Unhandled soot.Value type ${x.getClass}")
        Ast()
    }
  }

  def astForAssign(assignStmt: AssignStmt, order: Int): Ast = {
    Ast(
      NewCall()
        .name(Operators.assignment)
        .lineNumber(line(assignStmt))
        .columnNumber(column(assignStmt))
        .methodFullName(Operators.assignment)
        .order(order)
    )
      .withChild(astForValue(assignStmt.getLeftOp, 1))
      .withChild(astForValue(assignStmt.getRightOp, 2))
  }

  private def astForMethodReturn(methodDeclaration: SootMethod): Ast = {
    val typeFullName = registerType(methodDeclaration.getReturnType.toQuotedString)
    val methodReturnNode =
      NewMethodReturn()
        .order(methodDeclaration.getParameterCount + 2)
        .typeFullName(typeFullName)
        .code(methodDeclaration.getReturnType.toQuotedString)
        .lineNumber(line(methodDeclaration))
    Ast(methodReturnNode)
  }

  private def createMethodNode(
      methodDeclaration: SootMethod,
      typeDecl: RefType,
      childNum: Int
  ) = {
    val fullName = methodFullName(typeDecl, methodDeclaration)
    val code =
      s"${methodDeclaration.getReturnType.toQuotedString} ${methodDeclaration.getName}${paramListSignature(methodDeclaration, withParams = true)}"
    val methodNode = NewMethod()
      .name(methodDeclaration.getName)
      .fullName(fullName)
      .code(code)
      .signature(
        methodDeclaration.getReturnType.toQuotedString + paramListSignature(methodDeclaration)
      )
      .isExternal(false)
      .order(childNum)
      .filename(filename)
      .lineNumber(line(methodDeclaration))
      .columnNumber(column(methodDeclaration))
    methodNode
  }

  private def methodFullName(
      typeDecl: RefType,
      methodDeclaration: SootMethod
  ): String = {
    val typeName   = typeDecl.toQuotedString
    val returnType = methodDeclaration.getReturnType.toQuotedString
    val methodName = methodDeclaration.getName
    s"$typeName.$methodName:$returnType${paramListSignature(methodDeclaration)}"
  }

  private def paramListSignature(methodDeclaration: SootMethod, withParams: Boolean = false) = {
    val paramTypes = methodDeclaration.getParameterTypes.asScala.map(_.toQuotedString)
    if (!withParams) {
      "(" + paramTypes.mkString(",") + ")"
    } else {
      "(" + paramTypes.zipWithIndex.map(x => { s"${x._1} param${x._2 + 1}" }).mkString(", ") + ")"
    }
  }
}

object AstCreator {
  def line(node: Host): Option[Integer] = {
    Option(node.getJavaSourceStartLineNumber)
  }

  def column(node: Host): Option[Integer] = {
    Option(node.getJavaSourceStartColumnNumber)
  }

  def withOrder[T <: Any, X](nodeList: java.util.List[T])(f: (T, Int) => X): Seq[X] = {
    nodeList.asScala.zipWithIndex.map { case (x, i) =>
      f(x, i + 1)
    }.toSeq
  }

  def withOrder[T <: Any, X](nodeList: Iterable[T])(f: (T, Int) => X): Seq[X] = {
    nodeList.zipWithIndex.map { case (x, i) =>
      f(x, i + 1)
    }.toSeq
  }
}
