package wacc

import parsley.Parsley
import parsley.token.Lexer
import parsley.token.descriptions._

object lexer {
    private val desc = LexicalDesc.plain.copy(
        // your configuration goes here
        keywords =  Set("int ", "bool ", "char ", "string ", "pair ", "null ", "skip", "read ", "free ", "return ", 
                "exit ", "print", "println", "if ", "then ", "else ", "fi", "while ", "do ", "is",
                "done", "begin", "end", "call", "fst ", "snd ", "newpair", "true", "false",
                ";" , "(", ")", "{", "}", "[", "]", ",",
                "!","-", "len", "ord", "chr", 
                "+", "-", "*", "/", "%","<", ">", "<=", ">=", "=", "==", "!=","&&", "||")
    )
    
    private val lexer = new Lexer(desc)

    val integer = lexer.lexeme.integer.decimal
    val implicits = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
// input: String
// output: List[String(token)]
// example:
// input: int x = 1;
// output: ["int", "x", "=", "1", ";"]



    
