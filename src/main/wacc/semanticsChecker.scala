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


      case n@NewAssignment(identType, name, value) =>
        semanticCheck(name)
        semanticCheck(value)
        if (identType.getType != value.getType) {
          errors.append(SemanticError("assiment type mismatch"))
        }
        symbolTable.insertSymbol(lvalue, identType.getType) //should change type of SymbolTable to LValue

      case n@Assignment(lvalue, rvalue) =>
        // Check LValue and RValue
        semanticCheck(lvalue)
        semanticCheck(rvalue)
        //should change type of SymbolTable to LValue
        symbolTable.lookupSymbol(lvalue) match {
          case Some(symbolEntry) =>
            if (symbolEntry.varType != rvalue.getType) {
              errors.append(SemanticError("assiment type mismatch"))
            }
          case None => 
            errors.append(SemanticError("assiment value not exist"))
        }


      case n@ArrLiter(e, es) =>
        val ess = e::es
        ess.foreach(semanticCheck)
        if (ess.map(_.getType).distinct != 1) {
          errors.append(SemanticError("list elements type mismatch"))
        }
        else {
          n.getType = e.getType
        }

      case n@ArrElem(name, value) =>
        //check ident exist in symbolTable
        symbolTable.lookupSymbol(name) match {
                  case Some(symbolEntry) =>
                    n.getType = symbolEntry.varType
                  case None => 
                    errors.append(SemanticError("array not exist"))
                }
        //check each expr
        value.foreach(semanticCheck)
        //check each expr is type int
        if (!value.forall(x=> x.getType == "int")) {
          errors.append(SemanticError("index should be an Int"))
        }

      case n@If(condition, thenBranch, elseBranch) =>
        semanticCheck(condition)
        semanticCheck(thenBranch)
        semanticCheck(elseBranch)

      case n@While(condition, body) =>
        semanticCheck(condition)
        semanticCheck(body)

      case n@Print(expr, _) =>
        semanticCheck(expr)

      case n@Read(lvalue) =>
        semanticCheck(lvalue)

      case n@Free(expr) =>
        semanticCheck(expr)

      case n@Exit(expr) =>
        semanticCheck(expr)

      case n@NewPairRValue(exprL, exprR) =>
        semanticCheck(exprL)
        semanticCheck(exprR)
        n.getType = s"pair(${exprL.getType}, ${exprR.getType})"

      case n@CallRValue(func, args) =>
        semanticCheck(func)
        semanticCheck(args)
        //need to check each args's type is correct

        n.getType = ??? //set the type to func's resturn type
        

      case n@ArgList(exprl) =>
        exprl.foreach(semanticCheck)
      
      //-------Type-------
      case n@BaseType(name) => //doing nothing
      case n@ArrayType(elementType) =>
      case n@PairType(first, second) =>
      
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
            n.getType = getTypeForPair(symbolEntry.varType, 1)
          case None => 
            errors.append(SemanticError("Value not exist"))
        }
      
      case n@FstPairElem(values) =>
        symbolTable.lookupSymbol(values) match {
          case Some(symbolEntry) =>
            n.getType = getTypeForPair(symbolEntry.varType, 2)
          case None => 
            errors.append(SemanticError("Value not exist"))
        }

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

  //function to getType of Pair, input a string with form "pair(Type, Type)"
  //and a number which 1 represent fst, 2 respresent snd
  private def getTypeForPair(str: String, number: Int): String = {
    val startIndex = str.indexOf('(')
    val endIndex = str.indexOf(')')
    val substring = str.substring(startIndex + 1, endIndex)

    // Split the substring using comma and get the first part
    val typesArray = substring.split(',')
    if (number == 1) return typesArray(0)
    else return typesArray(1)
  }

}