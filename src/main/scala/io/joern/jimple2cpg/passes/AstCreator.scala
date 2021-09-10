package io.joern.jimple2cpg.passes

import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewBlock,
  NewCall,
  NewFieldIdentifier,
  NewIdentifier,
  NewLiteral,
  NewLocal,
  NewMember,
  NewMethod,
  NewMethodParameterIn,
  NewMethodReturn,
  NewNamespaceBlock,
  NewNode,
  NewReturn,
  NewTypeDecl,
  NewUnknown
}
import io.shiftleft.passes.DiffGraph
import io.shiftleft.x2cpg.Ast
import org.slf4j.LoggerFactory
import soot.jimple.{
  AddExpr,
  AndExpr,
  AnyNewExpr,
  AssignStmt,
  BinopExpr,
  CastExpr,
  CaughtExceptionRef,
  ClassConstant,
  CmpExpr,
  CmpgExpr,
  CmplExpr,
  Constant,
  DefinitionStmt,
  DivExpr,
  DoubleConstant,
  DynamicInvokeExpr,
  EqExpr,
  Expr,
  FieldRef,
  FloatConstant,
  GeExpr,
  GotoStmt,
  GtExpr,
  IdentityRef,
  IdentityStmt,
  IfStmt,
  InstanceFieldRef,
  InstanceInvokeExpr,
  InstanceOfExpr,
  IntConstant,
  InvokeExpr,
  InvokeStmt,
  LeExpr,
  LengthExpr,
  LongConstant,
  LookupSwitchStmt,
  LtExpr,
  MonitorStmt,
  MulExpr,
  NegExpr,
  NewArrayExpr,
  NewExpr,
  NullConstant,
  OrExpr,
  ParameterRef,
  RemExpr,
  ReturnStmt,
  ReturnVoidStmt,
  ShlExpr,
  ShrExpr,
  StaticFieldRef,
  StaticInvokeExpr,
  StringConstant,
  SubExpr,
  TableSwitchStmt,
  ThisRef,
  ThrowStmt,
  UshrExpr,
  VirtualInvokeExpr,
  XorExpr
}
import soot.tagkit.Host
import soot.{Body, Local, RefType, SootClass, SootField, SootMethod, Value}

import java.io.File
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.Try

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

    val memberAsts = typ.getSootClass.getFields.asScala
      .filter(_.isDeclared)
      .zipWithIndex
      .map { case (v, i) =>
        astForField(v, i + methodAsts.size + 1)
      }
      .toList

    Ast(typeDecl)
      .withChildren(memberAsts)
      .withChildren(methodAsts)
  }

  private def astForField(v: SootField, order: Int): Ast = {
    val typeFullName = registerType(v.getType.toQuotedString)
    val name         = v.getName
    Ast(
      NewMember()
        .name(name)
        .typeFullName(typeFullName)
        .order(order)
        .code(s"$typeFullName $name")
    )
  }

  private def astForMethod(
      methodDeclaration: SootMethod,
      typeDecl: RefType,
      childNum: Int
  ): Ast = {
    val methodNode = createMethodNode(methodDeclaration, typeDecl, childNum)
    val lastOrder  = 2 + methodDeclaration.getParameterCount
    try {
      val methodBody = methodDeclaration.retrieveActiveBody()
      val parameterAsts = withOrder(methodBody.getParameterLocals) { (p, order) =>
        astForParameter(p, order, methodDeclaration)
      }
      Ast(methodNode)
        .withChildren(parameterAsts)
        .withChild(astForMethodBody(methodBody, lastOrder))
        .withChild(astForMethodReturn(methodDeclaration))
    } catch {
      case e: RuntimeException =>
        logger.warn("Unable to parse method body.", e)
        Ast(methodNode)
          .withChild(astForMethodReturn(methodDeclaration))
    }
  }

  private def astForParameter(
      parameter: Local,
      childNum: Int,
      methodDeclaration: SootMethod
  ): Ast = {
    val typeFullName = registerType(parameter.getType.toQuotedString)
    val parameterNode = NewMethodParameterIn()
      .name(parameter.getName)
      .code(s"${parameter.getType.toQuotedString} ${parameter.getName}")
      .typeFullName(typeFullName)
      .order(childNum)
      .lineNumber(line(methodDeclaration))
      .columnNumber(column(methodDeclaration))
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
      case x: AssignStmt => astsForDefinition(x, order)
//      case x: IfStmt           => Seq()
//      case x: GotoStmt         => Seq()
      case x: IdentityStmt => astsForDefinition(x, order)
//      case x: LookupSwitchStmt => Seq()
//      case x: TableSwitchStmt  => Seq()
      case x: InvokeStmt     => astsForExpression(x.getInvokeExpr, order, statement)
      case x: ReturnStmt     => astsForReturnNode(x, order)
      case x: ReturnVoidStmt => astsForReturnVoidNode(x, order)
//      case x: ThrowStmt        => Seq()
//      case x: MonitorStmt      => Seq()
      case x =>
        logger.warn(s"Unhandled soot.Unit type ${x.getClass}")
        Seq()
    }
  }

  private def astForBinOpExpr(binOp: BinopExpr, order: Int, parentUnit: soot.Unit): Ast = {
    val operatorName = binOp match {
      case _: AddExpr  => Operators.addition
      case _: SubExpr  => Operators.subtraction
      case _: MulExpr  => Operators.multiplication
      case _: DivExpr  => Operators.division
      case _: RemExpr  => Operators.modulo
      case _: GeExpr   => Operators.greaterEqualsThan
      case _: GtExpr   => Operators.greaterThan
      case _: LeExpr   => Operators.lessEqualsThan
      case _: LtExpr   => Operators.lessThan
      case _: ShlExpr  => Operators.shiftLeft
      case _: ShrExpr  => Operators.logicalShiftRight
      case _: UshrExpr => Operators.arithmeticShiftRight
      case _: CmpExpr  => Operators.compare
      case _: CmpgExpr => Operators.compare
      case _: CmplExpr => Operators.compare
      case _: AndExpr  => Operators.and
      case _: OrExpr   => Operators.or
      case _: XorExpr  => Operators.xor
      case _: EqExpr   => Operators.equals
      case _           => ""
    }

    val callNode = NewCall()
      .name(operatorName)
      .methodFullName(operatorName)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .code(binOp.toString)
      .argumentIndex(order)
      .order(order)

    val args =
      astsForValue(binOp.getOp1, 1, parentUnit) ++ astsForValue(binOp.getOp2, 2, parentUnit)
    callAst(callNode, args)
  }

  private def astsForExpression(expr: Expr, order: Int, parentUnit: soot.Unit): Seq[Ast] = {
    expr match {
      case x: BinopExpr  => Seq(astForBinOpExpr(x, order, parentUnit))
      case x: InvokeExpr => Seq(astForInvokeExpr(x, order, parentUnit))
      case x: AnyNewExpr => Seq(astForNewExpr(x, order, parentUnit))
      case x: CastExpr   => Seq(astForUnaryExpr(Operators.cast, x, x.getOp, order, parentUnit))
      case x: InstanceOfExpr =>
        Seq(astForUnaryExpr(Operators.instanceOf, x, x.getOp, order, parentUnit))
      case x: LengthExpr =>
        Seq(astForUnaryExpr("<operator>.lengthOf", x, x.getOp, order, parentUnit))
      case x: NegExpr => Seq(astForUnaryExpr(Operators.minus, x, x.getOp, order, parentUnit))
      case x =>
        logger.warn(s"Unhandled soot.Expr type ${x.getClass}")
        Seq()
    }
  }

  private def astsForValue(value: soot.Value, order: Int, parentUnit: soot.Unit): Seq[Ast] = {
    value match {
      case x: Expr               => astsForExpression(x, order, parentUnit)
      case x: Local              => Seq(astForLocal(x, order, parentUnit))
      case x: IdentityRef        => Seq(astForIdentityRef(x, order, parentUnit))
      case x: Constant           => Seq(astForConstantExpr(x, order))
      case x: FieldRef           => Seq(astForFieldRef(x, order, parentUnit))
      case x: CaughtExceptionRef => Seq(astForCaughtExceptionRef(x, order, parentUnit))
      case x: ThisRef            => Seq(createThisNode(x))
      case x: ParameterRef       => Seq(createParameterNode(x, order))
      case x =>
        logger.warn(s"Unhandled soot.Value type ${x.getClass}")
        Seq()
    }
  }

  private def astForLocal(local: Local, order: Int, parentUnit: soot.Unit): Ast = {
    val name         = local.getName
    val typeFullName = registerType(local.getType.toQuotedString)
    Ast(
      NewIdentifier()
        .name(name)
        .lineNumber(line(parentUnit))
        .columnNumber(column(parentUnit))
        .order(order)
        .argumentIndex(order)
        .code(name)
        .typeFullName(typeFullName)
    )
  }

  private def astForIdentityRef(x: IdentityRef, order: Int, parentUnit: soot.Unit): Ast = {
    Ast(
      NewIdentifier()
        .code(x.toString())
        .name(x.toString())
        .order(order)
        .argumentIndex(order)
        .typeFullName(x.getType.toQuotedString)
        .lineNumber(line(parentUnit))
        .columnNumber(column(parentUnit))
    )
  }

  private def astForInvokeExpr(invokeExpr: InvokeExpr, order: Int, parentUnit: soot.Unit): Ast = {
    val dispatchType = invokeExpr match {
      case _: DynamicInvokeExpr => DispatchTypes.DYNAMIC_DISPATCH
      case _                    => DispatchTypes.STATIC_DISPATCH
    }
    val method = invokeExpr.getMethod
    val signature =
      s"${method.getReturnType.toQuotedString}(${(for (i <- 0 until method.getParameterCount)
        yield method.getParameterType(i).toQuotedString).mkString(",")})"
    val thisAsts = Seq(createThisNode(invokeExpr.getMethod))

    val callNode = NewCall()
      .name(method.getName)
      .code(s"${method.getName}(${invokeExpr.getArgs.asScala.mkString(", ")})")
      .dispatchType(dispatchType)
      .order(order)
      .argumentIndex(order)
      .methodFullName(s"${method.getDeclaringClass.toString}.${method.getName}:$signature")
      .signature(signature)
      .typeFullName(method.getDeclaringClass.getType.toQuotedString)
      .lineNumber(line(parentUnit))
      .columnNumber(column(parentUnit))

    val argAsts = withOrder(invokeExpr match {
      case x: DynamicInvokeExpr => x.getArgs.asScala ++ x.getBootstrapArgs.asScala
      case x                    => x.getArgs.asScala
    }) { case (arg, order) =>
      astsForValue(arg, order, parentUnit)
    }.flatten

    Ast(callNode)
      .withChildren(thisAsts)
      .withChildren(argAsts)
      .withArgEdges(callNode, thisAsts.flatMap(_.root))
      .withArgEdges(callNode, argAsts.flatMap(_.root))
  }

  private def astForNewExpr(x: AnyNewExpr, order: Int, parentUnit: soot.Unit): Ast = {
    Ast(
      NewUnknown()
        .typeFullName(x.getType.toQuotedString)
        .code("new")
        .order(order)
        .argumentIndex(order)
        .lineNumber(line(parentUnit))
        .columnNumber(column(parentUnit))
    )
  }

  private def astForUnaryExpr(
      methodName: String,
      unaryExpr: Expr,
      op: Value,
      order: Int,
      parentUnit: soot.Unit
  ): Ast = {
    val callBlock = NewCall()
      .name(methodName)
      .methodFullName(methodName)
      .code(unaryExpr.toString())
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .order(order)
      .typeFullName(unaryExpr.getType.toQuotedString)
      .argumentIndex(order)
      .lineNumber(line(parentUnit))
      .columnNumber(column(parentUnit))
    val valueAsts = astsForValue(op, 1, parentUnit)
    Ast(callBlock)
      .withChildren(valueAsts)
      .withArgEdges(callBlock, valueAsts.flatMap(_.root))
  }

  private def createThisNode(method: ThisRef): Ast = {
    Ast(
      NewIdentifier()
        .name("this")
        .code("this")
        .typeFullName(method.getType.toQuotedString)
        .order(0)
        .argumentIndex(0)
    )
  }

  private def createThisNode(method: SootMethod): Ast = {
    if (!method.isStatic) {
      Ast(
        NewIdentifier()
          .name("this")
          .code("this")
          .typeFullName(method.getDeclaringClass.getType.toQuotedString)
          .order(0)
          .argumentIndex(0)
      )
    } else {
      Ast()
    }
  }

  private def createParameterNode(parameterRef: ParameterRef, order: Int): Ast = {
    val name = s"@parameter${parameterRef.getIndex}"
    Ast(
      NewIdentifier()
        .name(name)
        .code(name)
        .typeFullName(parameterRef.getType.toQuotedString)
        .order(order)
        .argumentIndex(order)
    )
  }

  /** Creates the AST for assignment statements keeping in mind Jimple is a 3-address code language.
    */
  private def astsForDefinition(assignStmt: DefinitionStmt, order: Int): Seq[Ast] = {
    val leftOp       = assignStmt.getLeftOp.asInstanceOf[Local]
    val initializer  = assignStmt.getRightOp
    val name         = leftOp.getName
    val code         = leftOp.getType.toQuotedString + " " + leftOp.getName
    val typeFullName = registerType(leftOp.getType.toQuotedString)

    val identifier = astForLocal(leftOp, 1, assignStmt)
    val assignment = NewCall()
      .name(Operators.assignment)
      .code(s"$name = ${initializer.toString()}")
      .order(order)
      .argumentIndex(order)
      .typeFullName(assignStmt.getLeftOp.getType.toQuotedString)
      .build

    val initAsts       = astsForValue(initializer, 2, assignStmt)
    val initializerAst = Seq(callAst(assignment, Seq(identifier) ++ initAsts))
    Seq(
      Ast(
        NewLocal().name(name).code(code).typeFullName(typeFullName).order(order)
      )
    ) ++ initializerAst.toList
  }

  private def astsForReturnNode(returnStmt: ReturnStmt, order: Int): Seq[Ast] = {
    Seq(
      Ast(NewReturn().order(order).lineNumber(line(returnStmt)).columnNumber(column(returnStmt)))
        .withChildren(astsForValue(returnStmt.getOp, order + 1, returnStmt))
    )
  }

  private def astsForReturnVoidNode(returnVoidStmt: ReturnVoidStmt, order: Int): Seq[Ast] = {
    Seq(
      Ast(
        NewReturn()
          .order(order)
          .lineNumber(line(returnVoidStmt))
          .columnNumber(column(returnVoidStmt))
      )
    )
  }

  private def astForFieldRef(fieldRef: FieldRef, order: Int, parentUnit: soot.Unit): Ast = {
    val leftOpString = fieldRef match {
      case x: StaticFieldRef   => x.getFieldRef.declaringClass().toString
      case x: InstanceFieldRef => x.getBase.toString()
      case _                   => fieldRef.getFieldRef.declaringClass().toString
    }
    val leftOpType = fieldRef match {
      case x: StaticFieldRef   => x.getFieldRef.declaringClass().getType
      case x: InstanceFieldRef => x.getBase.getType
      case _                   => fieldRef.getFieldRef.declaringClass().getType
    }
    val fieldAccessBlock = NewCall()
      .name(Operators.fieldAccess)
      .code(s"${leftOpType.toQuotedString}.${fieldRef.getField.getName}")
      .typeFullName(fieldRef.getType.toQuotedString)
      .methodFullName(Operators.fieldAccess)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .order(order)
      .argumentIndex(order)
      .lineNumber(line(parentUnit))
      .columnNumber(column(parentUnit))
      .build

    val argAsts = Seq(
      NewIdentifier()
        .order(1)
        .argumentIndex(1)
        .lineNumber(line(parentUnit))
        .columnNumber(column(parentUnit))
        .name(leftOpString)
        .code(leftOpString)
        .typeFullName(leftOpType.toQuotedString),
      NewFieldIdentifier()
        .order(2)
        .argumentIndex(2)
        .lineNumber(line(parentUnit))
        .columnNumber(column(parentUnit))
        .canonicalName(fieldRef.getField.getSignature)
        .code(fieldRef.getField.getName)
    ).map(Ast(_))

    Ast(fieldAccessBlock)
      .withChildren(argAsts)
      .withArgEdges(fieldAccessBlock, argAsts.flatMap(_.root))
  }

  private def astForCaughtExceptionRef(
      caughtException: CaughtExceptionRef,
      order: Int,
      parentUnit: soot.Unit
  ): Ast = {
    Ast(
      NewIdentifier()
        .order(order)
        .argumentIndex(order)
        .lineNumber(line(parentUnit))
        .columnNumber(column(parentUnit))
        .name(caughtException.toString())
        .code(caughtException.toString())
        .typeFullName(caughtException.getType.toQuotedString)
    )
  }

  private def astForConstantExpr(constant: Constant, order: Int): Ast = {
    constant match {
      case _: ClassConstant => Ast()
      case _: NullConstant  => Ast()
      case _: IntConstant =>
        registerType("int")
        Ast(
          NewLiteral().order(order).argumentIndex(order).code(constant.toString).typeFullName("int")
        )
      case _: LongConstant =>
        registerType("long")
        Ast(
          NewLiteral()
            .order(order)
            .argumentIndex(order)
            .code(constant.toString)
            .typeFullName("long")
        )
      case _: DoubleConstant =>
        registerType("double")
        Ast(
          NewLiteral()
            .order(order)
            .argumentIndex(order)
            .code(constant.toString)
            .typeFullName("double")
        )
      case _: FloatConstant =>
        registerType("float")
        Ast(
          NewLiteral()
            .order(order)
            .argumentIndex(order)
            .code(constant.toString)
            .typeFullName("float")
        )
      case _: StringConstant =>
        registerType("java.lang.String")
        Ast(
          NewLiteral()
            .order(order)
            .argumentIndex(order)
            .code(constant.toString)
            .typeFullName("java.lang.String")
        )
    }
  }

  private def callAst(rootNode: NewNode, args: Seq[Ast]): Ast = {
    Ast(rootNode)
      .withChildren(args)
      .withArgEdges(rootNode, args.flatMap(_.root))
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
    NewMethod()
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
    val paramNames =
      if (!methodDeclaration.isPhantom)
        methodDeclaration.retrieveActiveBody().getParameterLocals.asScala.map(_.getName)
      else
        paramTypes.zipWithIndex.map(x => { s"${x._1} param${x._2 + 1}" })
    if (!withParams) {
      "(" + paramTypes.mkString(",") + ")"
    } else {
      "(" + paramTypes.zip(paramNames).map(x => s"${x._1} ${x._2}").mkString(", ") + ")"
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
