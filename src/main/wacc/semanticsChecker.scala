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
        semanticCheck(returnType)

        symbolTable.enterScope()
        semanticCheck(params)
        symbolTable.insertSymbolwithValue(functionName, "func", params.getType)
        symbolTable.enterFunc(returnType)
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
      
      case n@Exit(expr)=>
        semanticCheck(expr)
        if  (expr.getType != "int") {
          errors.append(SemanticError("exit code need to be Int"))
        }

      case n@SeqStmt(first, second) =>
        semanticCheck(first)
        semanticCheck(second)
        first match {
          case Return(_) => errors.append(SemanticError("return is not last statement of function"))
          case _ =>
        }

      case n@Begin(stmt) =>
        semanticCheck(stmt)

      case n@ParamList(params) =>
        params.foreach(semanticCheck)
      
      case n@Param(paramType, paramName) =>
        semanticCheck(paramType)
        symbolTable.insertSymbol(paramName, paramType.getType)

      case n@NewAssignment(identType, name, value) =>
        semanticCheck(name)
        semanticCheck(value)
        if (identType.getType != value.getType) {
          errors.append(SemanticError("assignment type mismatch"))
        }
        value match {
          case NewPairRValue(exprL, exprR) =>
            symbolTable.insertSymbolwithValue(name, identType.getType, List(exprL.getType, exprR.getType))
          case _ =>
            symbolTable.insertSymbol(name, identType.getType)
        }

      case n@Assignment(lvalue, rvalue) =>
        semanticCheck(lvalue)
        semanticCheck(rvalue)

        symbolTable.lookupSymbol(lvalue) match {
          case Some(symbolEntry) =>
            if (symbolEntry.varType != rvalue.getType) {
              errors.append(SemanticError("assignment type mismatch"))
            }
          case None => 
            errors.append(SemanticError("assignment value not exist"))
        }

      case n@ArrLiter(e, es) =>
        val ess = e::es
        if (e != StringLiter("empty")) {
          ess.foreach(semanticCheck)
          if (ess.map(_.getType).distinct != 1) {
            errors.append(SemanticError("list elements type mismatch"))
          }
        }
        n.getType = e.getType + "[]"


      case n@ArrElem(name, value) =>
        symbolTable.lookupSymbol(name) match {
          case Some(symbolEntry) =>
            n.getType = symbolEntry.varType
          case None => 
            errors.append(SemanticError("array not exist"))
        }
        value.foreach(semanticCheck)
        if (!value.forall(x=> x.getType == "int")) {
          errors.append(SemanticError("index should be an Int"))
        }

      case n@If(condition, thenBranch, elseBranch) =>
        semanticCheck(condition)
        if (condition.getType != "bool") {
          errors.append(SemanticError("condition need to be a boolean"))
        }
        symbolTable.enterScope()
        semanticCheck(thenBranch)
        symbolTable.exitScope()
        symbolTable.enterScope()
        semanticCheck(elseBranch)
        symbolTable.exitScope()
        //scoping

      case n@While(condition, body) =>
        semanticCheck(condition)
        if (condition.getType != "bool") {
          errors.append(SemanticError("condition need to be a boolean"))
        }
        symbolTable.enterScope()
        semanticCheck(body)
        symbolTable.exitScope()
        //scoping

      case n@Print(expr, _) =>
        semanticCheck(expr)

      case n@Read(lvalue) =>
        semanticCheck(lvalue)

      case n@Free(expr) =>
        semanticCheck(expr)

      case n@NewPairRValue(exprL, exprR) =>
        semanticCheck(exprL)
        semanticCheck(exprR)
        n.getType = s"pair(${exprL.getType}, ${exprR.getType})"

      case n@CallRValue(func, args) =>
        semanticCheck(func)
        semanticCheck(args)
        //need to check each args's type is correct
        symbolTable.lookupSymbol(func) match {
          case Some(symbolEntry) =>
            if (symbolEntry.value.length == args.exprl.length) {
              for (i <- 0 to symbolEntry.value.length) {
                if (symbolEntry.value(i) != args.exprl(i).getType) {
                  errors.append(SemanticError("function parameters type mismatch"))
                }
              }
            } else {
              errors.append(SemanticError("function has too many/few parameters"))
            }
          case None => 
            errors.append(SemanticError("function not exist"))
        }
        

      case n@ArgList(exprl) =>
        exprl.foreach(semanticCheck)
      
      //-------Type-------
      case n@BaseType(name) => //doing nothing
      case n@ArrayType(elementType) =>
      case n@PairType(first, second) =>

      // ---------EXPR---------
        // ---------binary---------
      case n@Add(expr1, expr2) =>
        semanticCheck(expr1)
        semanticCheck(expr2)
        if (expr1.getType != expr2.getType) {
          errors.append(SemanticError("expression type mismatch"))
        }
        else {
          n.getType = expr1.getType
        }
      
      case n@Sub(expr1, expr2) =>
        semanticCheck(expr1)
        semanticCheck(expr2)
        if (expr1.getType != expr2.getType) {
          errors.append(SemanticError("expression type mismatch"))
        }
        else {
          n.getType = expr1.getType
        }
      case n@Mul(expr1, expr2) =>
        semanticCheck(expr1)
        semanticCheck(expr2)
        if (expr1.getType != expr2.getType) {
          errors.append(SemanticError("expression type mismatch"))
        }
        else {
          n.getType = expr1.getType
        }
      
      case n@Div(expr1, expr2) =>
        semanticCheck(expr1)
        semanticCheck(expr2)
        if (expr1.getType != expr2.getType) {
          errors.append(SemanticError("expression type mismatch"))
        }
        else {
          n.getType = expr1.getType
        }
      case n@Mod(expr1, expr2) =>
        semanticCheck(expr1)
        semanticCheck(expr2)
        if (expr1.getType != expr2.getType) {
          errors.append(SemanticError("expression type mismatch"))
        }
        else {
          n.getType = expr1.getType
        }
      case n@LessThan(expr1, expr2) =>
        semanticCheck(expr1)
        semanticCheck(expr2)
        if (expr1.getType != expr2.getType) {
          errors.append(SemanticError("expression type mismatch"))
        }
        else {
          n.getType = expr1.getType
        }
      case n@LessThanEq(expr1, expr2) =>
        semanticCheck(expr1)
        semanticCheck(expr2)
        if (expr1.getType != expr2.getType) {
          errors.append(SemanticError("expression type mismatch"))
        }
        else {
          n.getType = expr1.getType
        }
      case n@GreaterThan(expr1, expr2) =>
        semanticCheck(expr1)
        semanticCheck(expr2)
        if (expr1.getType != expr2.getType) {
          errors.append(SemanticError("expression type mismatch"))
        }
        else {
          n.getType = expr1.getType
        }
      case n@GreaterThanEq(expr1, expr2) =>
        semanticCheck(expr1)
        semanticCheck(expr2)
        if (expr1.getType != expr2.getType) {
          errors.append(SemanticError("expression type mismatch"))
        }
        else {
          n.getType = expr1.getType
        }
      case n@Eq(expr1, expr2) =>
        semanticCheck(expr1)
        semanticCheck(expr2)
        if (expr1.getType != expr2.getType) {
          errors.append(SemanticError("expression type mismatch"))
        }
        else {
          n.getType = expr1.getType
        }
      case n@NotEq(expr1, expr2) =>
        semanticCheck(expr1)
        semanticCheck(expr2)
        if (expr1.getType != expr2.getType) {
          errors.append(SemanticError("expression type mismatch"))
        }
        else {
          n.getType = expr1.getType
        }
      case n@And(expr1, expr2) =>
        semanticCheck(expr1)
        semanticCheck(expr2)
        if (expr1.getType != expr2.getType) {
          errors.append(SemanticError("expression type mismatch"))
        }
        else {
          n.getType = expr1.getType
        }
      case n@Or(expr1, expr2) =>
        semanticCheck(expr1)
        semanticCheck(expr2)
        if (expr1.getType != expr2.getType) {
          errors.append(SemanticError("expression type mismatch"))
        }
        else {
          n.getType = expr1.getType
        }
          // ---------unary---------
      case n@Invert(expr) => 
        semanticCheck(expr)
        n.getType = expr.getType
      case n@Negate(expr) => 
        semanticCheck(expr)
        n.getType = expr.getType
      case n@Len(expr) => 
        semanticCheck(expr)
        n.getType = expr.getType
      case n@Ord(expr) => 
        semanticCheck(expr)
        n.getType = expr.getType
      case n@Chr(expr) => 
        semanticCheck(expr)
        n.getType = expr.getType
      
      case n@FstPairElem(values) =>
        symbolTable.lookupSymbol(values) match {
          case Some(symbolEntry) =>
            n.getType = symbolEntry.value(0)
          case None => 
            errors.append(SemanticError("Value not exist"))
        }
      
      case n@SndPairElem(values) =>
        symbolTable.lookupSymbol(values) match {
          case Some(symbolEntry) =>
            n.getType = symbolEntry.value(1)
          case None => 
            errors.append(SemanticError("Value not exist"))
        }

      case n@IntLiter(_) => // Literals don't need semantic checks

      case n@BoolLiter(_) => // Literals don't need semantic checks

      case n@CharLiter(_) => // Literals don't need semantic checks

      case n@StringLiter(_) => // Literals don't need semantic checks

      case n@PairLiter => // Literals don't need semantic checks

      case n@Ident(_) => // Identifiers don't need semantic checks

      case _ =>
      // Handle other cases if necessary
    }
      
  }

  def getSemanticErrors: List[SemanticError] = errors.toList
  

}