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
          semanticCheck(func)
        }
        semanticCheck(stmts)

      case n@Func(returnType, functionName, params, body) =>
        symbolTable.insertSymbol(functionName, "func")

        symbolTable.enterScope()
        symbolTable.enterFunc(returnType)

        // semanticCheck(params) MAY NOT NEED TO CHECK PARAMS ????????
        semanticCheck(body)

        symbolTable.exitFunc()
        symbolTable.exitScope()

      case n@Return(expr) =>
        semanticCheck(expr)
        if (!symbolTable.isInFunc()) {
          errors.append(SemanticError("unexpected return statement"))
        }
        if (expr.getType == symbolTable.getFuncType) {
          errors.append(SemanticError("return type does not match"))
        }

        // TODO consider if, for, while


      case n@SeqStmt(first, second) =>
        semanticCheck(first)
        semanticCheck(second)

      case n@Begin(stmt) =>
        semanticCheck(stmt)

      case n@ParamList(params) =>
        params.foreach(semanticCheck)

      case n@Assignment(lvalue, rvalue) =>
        // Check LValue and RValue
        

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

      case n@Exit(expr) =>
        semanticCheck(expr)

      case n@NewPairRValue(exprL, exprR) =>
        semanticCheck(exprL)
        semanticCheck(exprR)

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

      case _ =>
      // Handle other cases if necessary
    }
      
  }

  private def checkFunc(func: Func): Unit = {
    // return type check

    // return code check

    // return what check
  }


  private def typeCheck(var1: ASTNode, var2: ASTNode): Boolean = {
    true
  }

  def getSemanticErrors: List[SemanticError] = errors.toList
}