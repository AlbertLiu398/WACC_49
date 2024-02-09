package wacc

import scala.collection.mutable.ListBuffer
import ast._

case class SemanticError(message: String)

class semanticsChecker(symbolTable: SymbolTable) {

  private val errors: ListBuffer[SemanticError] = ListBuffer()

  // Recursively semantic check all the ASTNode 
  def semanticCheck(ast: ASTNode): Unit = {

    ast match {
      case Program(funcList, stmts) =>
        symbolTable.enterScope()
        for (func <- funcList) {
          symbolTable.insertSymbolwithValue(func.functionName, "func", func.params.getType :+ func.returnType.getType)
        }
        for (func <- funcList) {
          semanticCheck(func)
        }
        semanticCheck(stmts)
        symbolTable.exitScope()

      case n@Func(returnType, functionName, params, body) =>
        semanticCheck(returnType)
        
        symbolTable.enterScope()
        symbolTable.enterFunc(returnType)
        semanticCheck(params)
        semanticCheck(body)
        symbolTable.exitFunc()
        symbolTable.exitScope()

      case n@Return(expr) =>
        semanticCheck(expr)
        if (!symbolTable.isInFunc()) {
          errors.append(SemanticError("unexpected return statement"))
        }
        if (!compareType(expr.getType, symbolTable.getFuncType)) {
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

      case n@Begin(stmt) =>
        symbolTable.enterScope()
        semanticCheck(stmt)
        symbolTable.exitScope()

      case n@ParamList(params) =>
        val identNames = params.map(_.paramName.value)
        if (identNames.distinct != identNames) {
          errors.append(SemanticError("functions arguments names overlap"))
        }
        params.foreach(semanticCheck)
      
      case n@Param(paramType, paramName) =>
        semanticCheck(paramType)
        symbolTable.insertSymbol(paramName, paramType.getType)

      case n@NewAssignment(identType, name, value) =>
        symbolTable.lookupSymbol(name) match {
          case Some(_) =>
            errors.append(SemanticError("variable name already exists"))
          case None =>
        }
        semanticCheck(identType)
        semanticCheck(value)
        value match {
          case ArrLiter(StringLiter("empty"), es) => 
          case _ =>
            if (!compareType(identType.getType, value.getType)) {
              errors.append(SemanticError("assignment type mismatch"))
            }
        }
        value match {
          case PairLiter => 
            symbolTable.insertSymbolwithValue(name, identType.getType, List(identType.getFst, identType.getSnd))
          case ArrLiter(x@Ident(nameI), es) => 
            if (x.getType.startsWith("pair")){
              symbolTable.lookupSymbol(x) match {
                case Some(entry) => 
                  symbolTable.insertSymbolwithValue(name, identType.getType, List(entry.value(0), entry.value(1)))
                case None => errors.append(SemanticError("Ident not exist"))
              }
            }
            else {
              symbolTable.insertSymbol(name, identType.getType)
            }
          case _ => 
            if (value.getType.startsWith("pair")) {
              if (value.getType == "pair") {
                symbolTable.insertSymbolwithValue(name, identType.getType, List(identType.getFst, identType.getSnd))
              }
              else {
                symbolTable.insertSymbolwithValue(name, value.getType, List(value.getFst, value.getSnd))
              }
            } else {
              symbolTable.insertSymbol(name, identType.getType)
            }
        }
       

      case n@Assignment(lvalue, rvalue) =>
        semanticCheck(lvalue)
        semanticCheck(rvalue)
        rvalue match {
          case ArrLiter(StringLiter("empty"), es) => 
          case _ =>
            if (!compareType(lvalue.getType,rvalue.getType)) {
              errors.append(SemanticError("assignment type mismatch"))
            }
        }


      case n@ArrLiter(e, es) =>
        val ess = e::es
        if (e != StringLiter("empty")) {
          ess.foreach(semanticCheck)
          val types = ess.map(_.getType).distinct
          val charAndString = types.length == 2 & types == List("char[]", "string")
          if (types.length != 1 & !charAndString) {
            errors.append(SemanticError("list elements type mismatch"))
          }
          if (charAndString) {
            n.getType = "string[]"
          }
          else {
            n.getType = e.getType + "[]"
          }
        }
        

      case n@ArrElem(name, value) =>
        var arryType = ""
        symbolTable.lookupSymbol(name) match {
          case Some(symbolEntry) =>
            n.getType = symbolEntry.varType.dropRight(value.length * 2)
            arryType = symbolEntry.varType
            if (n.getType.startsWith("pair")){
              n.getFst = symbolEntry.value(0)
              n.getSnd = symbolEntry.value(1)
            }
          case None => 
            symbolTable.displaySymbolTable()
            errors.append(SemanticError("array not exist"))
        }
        value.foreach(semanticCheck)
        if (!value.forall(x=> x.getType == "int")) {
          errors.append(SemanticError("index should be an Int"))
        }
        if (value.length > countOccurrences(arryType, "[]")) {
          println(value.length)
          println(n.getType)
          println(countOccurrences(n.getType, "[]"))
          errors.append(SemanticError("too much indexing"))
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
        println(lvalue.getType)
        if (lvalue.getType != "int" & lvalue.getType != "char") {
          errors.append(SemanticError("can only read int or char"))
        }

      case n@Free(expr) =>
        semanticCheck(expr)

      case n@NewPairRValue(exprL, exprR) =>
        semanticCheck(exprL)
        semanticCheck(exprR)
        n.getType = s"pair(${exprL.getType},${exprR.getType})"
        n.getFst = exprL.getType
        n.getSnd= exprR.getType

      case n@CallRValue(func, args) =>
        semanticCheck(args)
        //need to check each args's type is correct
        symbolTable.lookupSymbol(Ident('f' +: func.value)) match {
          case Some(symbolEntry) =>
            if (symbolEntry.varType == "func") {
              if (symbolEntry.value.length - 1 == args.exprl.length) {
                for (i <- 0 to args.exprl.length - 1) {
                  if (!compareType(symbolEntry.value(i), args.exprl(i).getType)) {
                    errors.append(SemanticError("function parameters type mismatch"))
                  }
                }
                n.getType = symbolEntry.value(symbolEntry.value.length - 1)
              } else {
                errors.append(SemanticError("function has too many/few parameters"))
              }
            } else {
              errors.append(SemanticError("calling argument is not function"))
            }
          case None => 
            errors.append(SemanticError("function does not exist"))
        }
        

      case n@ArgList(exprl) =>
        exprl.foreach(semanticCheck)
      
      //-------Type-------
      case n@BaseType(name) => //doing nothing
      case n@ArrayType(elementType) =>
      case n@PairType(first, second) =>
        n.getFst = first.getType
        n.getSnd = second.getType

      // ---------EXPR---------
        // ---------binary---------
      case n@Add(expr1, expr2) =>
        semanticCheck(expr1)
        semanticCheck(expr2)
        if (!compareType(expr1.getType,expr2.getType)) {
          errors.append(SemanticError("expression type mismatch"))
        }
        else {
          n.getType = expr1.getType
        }
      
      case n@Sub(expr1, expr2) =>
        semanticCheck(expr1)
        semanticCheck(expr2)
        if (!compareType(expr1.getType,expr2.getType)) {
          errors.append(SemanticError("expression type mismatch"))
        }
        else {
          n.getType = expr1.getType
        }
      case n@Mul(expr1, expr2) =>
        semanticCheck(expr1)
        semanticCheck(expr2)
        if (!compareType(expr1.getType,expr2.getType)) {
          errors.append(SemanticError("expression type mismatch"))
        }
        else {
          n.getType = expr1.getType
        }
      
      case n@Div(expr1, expr2) =>
        semanticCheck(expr1)
        semanticCheck(expr2)
        if (!compareType(expr1.getType,expr2.getType)) {
          errors.append(SemanticError("expression type mismatch"))
        }
        else {
          n.getType = expr1.getType
        }
      case n@Mod(expr1, expr2) =>
        semanticCheck(expr1)
        semanticCheck(expr2)
        if (!compareType(expr1.getType,expr2.getType)) {
          errors.append(SemanticError("expression type mismatch"))
        }
        else {
          n.getType = expr1.getType
        }
      case n@LessThan(expr1, expr2) =>
        semanticCheck(expr1)
        semanticCheck(expr2)
        if (!compareType(expr1.getType,expr2.getType)) {
          errors.append(SemanticError("expression type mismatch"))
        }
        else {
          if (expr1.getType.contains("pair") | expr1.getType.contains("[]")) {
            errors.append(SemanticError(s"cannot compare ${expr1.getType}"))
          }
          n.getType = "bool"
        }
      case n@LessThanEq(expr1, expr2) =>
        semanticCheck(expr1)
        semanticCheck(expr2)
        if (!compareType(expr1.getType,expr2.getType)) {
          errors.append(SemanticError("expression type mismatch"))
        }
        else {
          if (expr1.getType.contains("pair") | expr1.getType.contains("[]")) {
            errors.append(SemanticError(s"cannot compare ${expr1.getType}"))
          }
          n.getType = "bool"
        }
      case n@GreaterThan(expr1, expr2) =>
        semanticCheck(expr1)
        semanticCheck(expr2)
        if (!compareType(expr1.getType,expr2.getType)) {
          errors.append(SemanticError("expression type mismatch"))
        }
        else {
          if (expr1.getType.contains("pair") | expr1.getType.contains("[]")) {
            errors.append(SemanticError(s"cannot compare ${expr1.getType}"))
          }
          n.getType = "bool"
        }
      case n@GreaterThanEq(expr1, expr2) =>
        semanticCheck(expr1)
        semanticCheck(expr2)
        if (!compareType(expr1.getType,expr2.getType)) {
          errors.append(SemanticError("expression type mismatch"))
        }
        else {
          if (expr1.getType.contains("pair") | expr1.getType.contains("[]")) {
            errors.append(SemanticError(s"cannot compare ${expr1.getType}"))
          }
          n.getType = "bool"
        }
      case n@Eq(expr1, expr2) =>
        semanticCheck(expr1)
        semanticCheck(expr2)
        if (!compareType(expr1.getType,expr2.getType)) {
          errors.append(SemanticError("expression type mismatch"))
        }
        else {
          n.getType = "bool"
        }
      case n@NotEq(expr1, expr2) =>
        semanticCheck(expr1)
        semanticCheck(expr2)
        if (!compareType(expr1.getType,expr2.getType)) {
          errors.append(SemanticError("expression type mismatch"))
        }
        else {
          n.getType = "bool"
        }
      case n@And(expr1, expr2) =>
        semanticCheck(expr1)
        semanticCheck(expr2)
        if (expr1.getType != "bool" | expr2.getType != "bool") {
          errors.append(SemanticError("expression type mismatch"))
        }
        else {
          n.getType = "bool"
        }
      case n@Or(expr1, expr2) =>
        semanticCheck(expr1)
        semanticCheck(expr2)
        if (expr1.getType != "bool" | expr2.getType != "bool") {
          errors.append(SemanticError("expression type mismatch"))
        }
        else {
          n.getType = "bool"
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
        n.getType = "int"
      case n@Ord(expr) => 
        semanticCheck(expr)
        n.getType = "int"
      case n@Chr(expr) => 
        semanticCheck(expr)
        n.getType = "char"


      case n@FstPairElem(values) =>
        semanticCheck(values)
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
      case PairTypeElem => 

      case n@IntLiter(_) => // Literals don't need semantic checks

      case n@BoolLiter(_) => // Literals don't need semantic checks

      case n@CharLiter(_) => // Literals don't need semantic checks

      case n@StringLiter(_) => // Literals don't need semantic checks

      case n@PairLiter => // Literals don't need semantic checks

      case n@Ident(_) =>
        symbolTable.lookupSymbol(n) match {
          case Some(entry) => 
            n.getType = entry.varType
            if (entry.varType.startsWith("pair")) {
              n.getFst = entry.value(0)
              n.getSnd = entry.value(1)
            }
          case None => errors.append(SemanticError("Ident not exist"))
        }
  

      case _ =>
      // Handle other cases if necessary
    }
      
  }

  // Make errors to list
  def getSemanticErrors: List[SemanticError] = errors.toList
  
  // Clean errors
  def refreshSymbolTable(): Unit  = {
    errors.clear()
  }
  // Use to calculate the dimension of array
  def countOccurrences(mainString: String, subString: String): Int = {
    return mainString.sliding(subString.length).count(window => window == subString)
  }
  // Use to compareType, make char[] and string eqauls
  def compareType(s1: String, s2: String): Boolean = {
    var fstStr = s1
    var sndStr = s2
    if  (s1.startsWith("pair") & s2.startsWith("pair")) return true
    if (s1 == "char[]") {
      fstStr = "string"
    }
    if (s2 == "char[]") {
      sndStr = "string"
    }
    return fstStr == sndStr
  }


}