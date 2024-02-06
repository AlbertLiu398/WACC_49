package wacc

import scala.collection.mutable._
import ast._

case class SymbolEntry(name: String, varType: String)

class SymbolTable {

  private val scopeStack: Stack[Map[String, ListBuffer[SymbolEntry]]] = Stack(Map())

  def insertSymbol(name: String, varType: String): Unit = {
    val symbolEntry = SymbolEntry(name, varType)
    val currentScopeMap = scopeStack.top
    if (currentScopeMap.contains(name)) {
      // produce error !
    } else {
      currentScopeMap(name) = ListBuffer(symbolEntry)
    }
  }

  def lookupSymbol(name: String): Option[SymbolEntry] = {
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