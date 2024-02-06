package wacc

import scala.collection.mutable.ListBuffer
import ast._

case class SemanticError(message: String)

class semanticsChecker(symbolTable: SymbolTable) {

  private val errors: ListBuffer[SemanticError] = ListBuffer()

  def semanticCheck(ast: ASTNode): Unit = {

    ast match {

      case Program(funcList, stmts) =>
        for (func <- funcList) {
          checkFunc(func)
        }
        semanticCheck(stmts)

      case n@SeqStmt(first, second) =>
        semanticCheck(first)
        semanticCheck(second)

      case n@Begin(stmt) =>
        semanticCheck(stmt)

      case n@Func(returnType, functionName, params, body) =>
        semanticCheck(params)

        //symbolTable.enterScope()
        //params.paramListType.foreach(param => symbolTable.addVariable(param.paramName.value, param.paramType))
        semanticCheck(body)
        //symbolTable.exitScope()

      case n@ParamList(params) =>
        params.foreach(semanticCheck)

      case n@Assignment(lvalue, rvalue) =>
        // Check LValue and RValue
        semanticCheck(lvalue)
        semanticCheck(rvalue)

      case n@IdentLValue(name) =>
        // Check if the variable exists in the symbol table
        //if (!symbolTable.isDefined(name.value)) {
        //  errors += SemanticError(s"Variable '${name.value}' is not defined.")
        //}

      case n@UnaryOperation(operator, expr) =>
        semanticCheck(expr)

      case n@BinaryOperation(operator, left, right) =>
        semanticCheck(left)
        semanticCheck(right)

      case n@ArrLiter(e, es) =>
        semanticCheck(e)
        es.foreach(semanticCheck)

      case n@ArrElem(name, value) =>
        semanticCheck(name)
        value.foreach(semanticCheck)

      case n@If(condition, thenBranch, elseBranch) =>
        semanticCheck(condition)
        semanticCheck(thenBranch)
        semanticCheck(elseBranch)

      case n@While(condition, body) =>
        semanticCheck(condition)
        semanticCheck(body)

      case n@Print(expr, _) =>
        semanticCheck(expr)

      case n@NewAssignment(identType, name, value) =>
        semanticCheck(name)
        semanticCheck(value)

      case n@Read(lvalue) =>
        semanticCheck(lvalue)

      case n@Free(expr) =>
        semanticCheck(expr)

      case n@Return(expr) =>
        semanticCheck(expr)

      case n@Exit(expr) =>
        semanticCheck(expr)

      case n@NewPairRValue(exprL, exprR) =>
        semanticCheck(exprL)
        semanticCheck(exprR)

      case n@ArrayLiterRValue(expressions) =>
        semanticCheck(expressions)

      case n@CallRValue(func, args) =>
        semanticCheck(func)
        semanticCheck(args)

      case n@IntLiter(_) => // Literals don't need semantic checks

      case n@BoolLiter(_) => // Literals don't need semantic checks

      case n@CharLiter(_) => // Literals don't need semantic checks

      case n@StringLiter(_) => // Literals don't need semantic checks

      case n@PairLiter => // Literals don't need semantic checks

      case n@Ident(_) => // Identifiers don't need semantic checks

      case n@Param(_, _) => // Parameters don't need semantic checks

      case n@PairElem(_, values) =>
        semanticCheck(values)

      case _ =>
      // Handle other cases if necessary
    }
      
  }

  private def checkFunc(func: Func): Unit = {

  }

  private def typeCheck(var1: ASTNode, var2: ASTNode): Boolean = {
    true
  }

  def getSemanticErrors: List[SemanticError] = errors.toList
}