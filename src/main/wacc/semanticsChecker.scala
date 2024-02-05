import scala.collection.mutable.ListBuffer

case class SemanticError(message: String)

class semanticsChecker(symbolTable: SymbolTable) {

  private val errors: ListBuffer[SemanticError] = ListBuffer()

  def analyzeAST(ast: List[ASTNode]): Unit = {
    for (node <- ast) {
      node match {
        case VariableDeclarationNode(name, varType, scope) =>
          if (!isValidType(varType)) {
            errors += SemanticError(s"Invalid variable type '$varType' for variable '$name' in scope '$scope'.")
          }
          if (symbolTable.lookupSymbol(name, scope).isDefined) {
            errors += SemanticError(s"Variable '$name' is already declared in scope '$scope'.")
          } else {
            symbolTable.insertSymbol(name, varType, scope)
          }

        case VariableReferenceNode(name, scope) =>
          if (symbolTable.lookupSymbol(name, scope).isEmpty) {
            errors += SemanticError(s"Variable '$name' is undeclared in scope '$scope'.")
          }

        case _ =>

      }
    }
  }

  private def isValidType(varType: String): Boolean = {
    true
  }

  def getSemanticErrors: List[SemanticError] = errors.toList
}