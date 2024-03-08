package wacc

import scala.collection.mutable._
import ast._
import parsley.expr.infix



/* symbol table stores 
   variables, 
   functions, 
   or other symbols declared in your program*/


// Symbol table entry that keeps track of ident name and its type
// 'value' is used when creating pair and array type entries
case class SymbolEntry(name: Ident, varType: String, value: List[String])

class SymbolTable {

  // Stack to keep track of symbol table states when entering/exiting scope
  private val scopeStack: Stack[Map[Ident, ListBuffer[SymbolEntry]]] = Stack(Map())
  private var inFunc: Boolean = false
  private var funcType: String = "notInFunc"

  private val funcMap = Map[Ident, ListBuffer[SymbolEntry]]()
  
  // Shows number of variables in any functions and in main
  private val varList: ListBuffer[Int] = ListBuffer()
  private var varCounter = 0

  def getIdent(lvalue: LValue): Ident = {
    lvalue match {
      case n@Ident(value) => return n
      case n@ArrElem(name, value) => return name
      case n@FstPairElem(values) => return getIdent(values)
      case n@SndPairElem(values) => return getIdent(values)
    }
  }

  // Insert a symbol table entry without value
  def insertSymbol(value_name: LValue, varType: String): Boolean = {
    varCounter += 1
    val name = getIdent(value_name)
    val symbolEntry = SymbolEntry(name, varType, Nil)
    val currentScopeMap = scopeStack.top
    if (currentScopeMap.contains(name)) {
      if(isInFunc()) {
        currentScopeMap.remove(name)
      } else {
        return false
      }
    }
    currentScopeMap(name) = ListBuffer(symbolEntry)
    return true
  }
  
  // Insert a symbol table entry with value
  def insertSymbolwithValue(value_name: LValue, varType: String, value: List[String]): Boolean = {
    varCounter += 1
    print("before inserting the scope is: " + scopeStack + "\n")
    var name = Ident(getIdent(value_name).value)

    /*  ------------ insertion rule :
     when varType is function, we could have multiple functions with the same name
     but when varType is not function, we should not have multiple variables with the same name */

    if (varType != "func" && scopeStack.exists(_.contains(name))) {
      return false
    }
    if (varType == "func") {
      name = Ident('f' +: name.value)
    }
    val symbolEntry = SymbolEntry(name, varType, value)
    val currentScopeMap = scopeStack.top
    if (currentScopeMap.contains(name)) {
      // If it's a function, add the new overload
      if (varType == "func") {
        currentScopeMap(name) += symbolEntry
      } else {
        // If it's not a function, return false (duplicate variable)
        return false
      }
    } else {
      // If the symbol doesn't exist in the current scope, add it
      currentScopeMap(name) = ListBuffer(symbolEntry)
    }
    print("after insert now the scope is: " + scopeStack + "\n")
    return true
  }



  def lookupSymbol(value_name: LValue): Option[SymbolEntry] = {
    val name = getIdent(value_name)
    scopeStack.find(_.contains(name)) match {
      case Some(scopeMap) => scopeMap.get(name).flatMap(_.headOption)
      case None => None
    }
  }

  // Method to lookup all overloads of a function
  def lookupFunctionOverloads(funcName: Ident): Option[List[SymbolEntry]] = {
    val name = Ident('f' +: funcName.value)
    scopeStack.find(_.contains(name)) match {
      case Some(scopeMap) => {
        print(" now i find the function " + name + "\n" )
        val funcEntries = scopeMap(name).filter(_.varType == "func")
        if (funcEntries.isEmpty) {
          None
        } else {
          print("funcEntries.toList: " + funcEntries.toList + "\n")
          Some(funcEntries.toList)
        }
      }
      case None => None
    }
  }

  // Enter a new scope by pushing an empty map onto the scope stack
  def enterScope(): Unit = {
    scopeStack.push(Map())
  }

  // Exit the current scope by popping the top map from the scope stack
  def exitScope(): Unit = {
    scopeStack.pop()
  }

  // Enter a new function scope by pushing an empty map onto the scope stack
  def enterFunc(returnType: Type): Unit = {
    inFunc = true
    funcType = returnType.getType
  }

  // Exit the current function scope by popping the top map from the scope stack
  def exitFunc(): Unit = {
    inFunc = false
    funcType = "notInFunc"
    varList.append(varCounter)
    varCounter = 0
  }

  def isInFunc(): Boolean = {
    return inFunc
  }

  def getFuncType: String = {
    return funcType
  }

  def checkDoubleDeclear(ident: Ident) : Boolean = {
    return scopeStack.top.contains(ident)
  }

  def exitMain(funcList: List[Func]): Unit = {
    varList.append(varCounter)
    varList(0) -= funcList.length
    for (i <- 0 until varList.length - 1) {
      varList(i) -= funcList(i).params.paramListType.length
    }
  }

  def getVarList(): List[Int] = varList.toList

  def displaySymbolTable(): Unit = {
    for (scopeMap <- scopeStack) {
      for ((name, entries) <- scopeMap) {
        for (entry <- entries) {
          println(s"Name: $name, Type: ${entry.varType}, Value: ${entry.value}")
        }
      }
    }
  }

  // clean all contents in the symbol table
  def clean() : Unit = {
    scopeStack.clear()
    scopeStack.push(Map())
    inFunc = false
    funcType = "notInFunc"
    varList.clear()
    varCounter = 0
  }
}