import scala.collection.mutable._

case class SymbolEntry(name: String, varType: String, scope: String)

class SymbolTable {

  private val symbolMap: Map[String, ListBuffer[SymbolEntry]] = Map()

  def insertSymbol(name: String, varType: String, scope: String): Unit = {
    val symbolEntry = SymbolEntry(name, varType, scope)
    if (symbolMap.contains(name)) {
      symbolMap(name) += symbolEntry
    } else {
      symbolMap(name) = ListBuffer(symbolEntry)
    }
  }

  def lookupSymbol(name: String, scope: String): Option[SymbolEntry] = {
    symbolMap.get(name).flatMap { entries =>
      entries.find(_.scope == scope)
    }
  }

  def displaySymbolTable(): Unit = {
    for ((name, entries) <- symbolMap) {
      for (entry <- entries) {
        println(s"Name: $name, Type: ${entry.varType}, Scope: ${entry.scope}")
      }
    }
  }
}

// Example usage
// val symbolTable = new SymbolTable()

// Insert symbols into the symbol table
// symbolTable.insertSymbol("x", "Int", "Global")
// symbolTable.insertSymbol("y", "String", "Local")
// symbolTable.insertSymbol("x", "Double", "Function1")

// Lookup symbols in the symbol table
// val symbol1 = symbolTable.lookupSymbol("x", "Global")
// val symbol2 = symbolTable.lookupSymbol("y", "Local")
// val symbol3 = symbolTable.lookupSymbol("x", "Function1")

// Display the contents of the symbol table
// symbolTable.displaySymbolTable()
