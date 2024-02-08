package wacc

import scala.collection.mutable._
import ast._
import parsley.expr.infix

case class SymbolEntry(name: Ident, varType: String, value: List[String])


//function: list = params type
//Pair: List[0] = fst type, List[1] = snd type
class SymbolTable {

  private val scopeStack: Stack[Map[Ident, ListBuffer[SymbolEntry]]] = Stack(Map())
  private var inFunc: Boolean = false
  private var funcType: String = "notInFunc"

  def getIdent(lvalue: LValue): Ident = {
    lvalue match {
      case n@Ident(value) => return n
      case n@ArrElem(name, value) => return name
      case n@FstPairElem(values) => return getIdent(values)
      case n@SndPairElem(values) => return getIdent(values)
    }
  }

  
  def insertSymbol(value_name: LValue, varType: String): Boolean = {
    val name = getIdent(value_name)
    val symbolEntry = SymbolEntry(name, varType, Nil)
    val currentScopeMap = scopeStack.top
    if (currentScopeMap.contains(name)) {
      return false
    }
    currentScopeMap(name) = ListBuffer(symbolEntry)
    return true
  }

  def insertSymbolwithValue(value_name: LValue, varType: String, value: List[String]): Boolean = {
    val name = getIdent(value_name)
    val symbolEntry = SymbolEntry(name, varType, value)
    val currentScopeMap = scopeStack.top
    if (currentScopeMap.contains(name)) {
      return false
    }
    currentScopeMap(name) = ListBuffer(symbolEntry)
    return true
  }



  def lookupSymbol(value_name: LValue): Option[SymbolEntry] = {
    val name = getIdent(value_name)
    scopeStack.find(_.contains(name)) match {
      case Some(scopeMap) => scopeMap.get(name).flatMap(_.headOption)
      case None => None
    }
  }

  def enterScope(): Unit = {
    scopeStack.push(Map())
  }

  def exitScope(): Unit = {
    scopeStack.pop()
  }

  def enterFunc(returnType: Type): Unit = {
    inFunc = true
    funcType = returnType.getType
  }


  def exitFunc(): Unit = {
    inFunc = false
    funcType = "notInFunc"
  }

  def isInFunc(): Boolean = {
    return inFunc
  }

  def getFuncType: String = {
    return funcType
  }

  def displaySymbolTable(): Unit = {
    for (scopeMap <- scopeStack) {
      for ((name, entries) <- scopeMap) {
        for (entry <- entries) {
          println(s"Name: $name, Type: ${entry.varType}, Value: ${entry.value}")
        }
      }
    }
  }
}