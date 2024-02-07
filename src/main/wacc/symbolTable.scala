package wacc

import scala.collection.mutable._
import ast._
import parsley.expr.infix

case class SymbolEntry(name: Ident, varType: String)

class SymbolTable {

  private val scopeStack: Stack[Map[Ident, ListBuffer[SymbolEntry]]] = Stack(Map())
  private var inFunc: Boolean = false
  private var funcType: String = "notInFunc"

  def insertSymbol(name: Ident, varType: String): Unit = {
    val symbolEntry = SymbolEntry(name, varType)
    val currentScopeMap = scopeStack.top
    if (currentScopeMap.contains(name)) {
      // produce error !
    } else {
      currentScopeMap(name) = ListBuffer(symbolEntry)
    }
  }

  def lookupSymbol(name: Ident): Option[SymbolEntry] = {
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
    funcType = enterFuncHelper(returnType)
  }

  private def enterFuncHelper(returnType: Type): String = {
    returnType match {
      case BaseType(name) =>
        return name
      case ArrayType(elementType) =>
        return "arr[" + enterFuncHelper(elementType) + "]"
      case PairType(first, second) =>
        return "pair" // TODO
    }
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
          println(s"Name: $name, Type: ${entry.varType}")
        }
      }
    }
  }
}