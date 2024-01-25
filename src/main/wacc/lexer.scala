package wacc

import parsley.Parsley
import parsley.token.{Lexer, predicate}
import parsley.token.descriptions._
import scala.collection.mutable.ListBuffer

object lexer {
    private val desc = LexicalDesc.plain.copy(
        // your configuration goes here
        nameDesc = NameDesc.plain.copy(
            identifierStart = predicate.Basic(_.isLetterOrDigit),
            identifierLetter = predicate.Basic(_.isLetterOrDigit),
        ),
        symbolDesc = SymbolDesc.plain.copy(
            caseSensitive = true,
            hardKeywords = Set("null ", "skip", "read ", "free ", "return ", "exit ", "print", "println", 
            "if ", "then ", "else ", "fi", "while ", "do ", "is","done", "begin", "end", "call", "fst ", 
            "snd ", "newpair", "true", "false",";" , "(", ")", "{", "}", "[", "]", ","),
            hardOperators = Set("!","-", "len", "ord", "chr", 
            "+", "-", "*", "/", "%","<", ">", "<=", ">=", "=", "==", "!=","&&", "||"),

        ),
        spaceDesc = SpaceDesc.plain.copy(
            lineCommentStart = "//",
            lineCommentAllowsEOF = true,
            multiLineCommentStart = "/*", 
            multiLineCommentEnd = "*/",
            multiLineNestedComments = false,
            whitespaceIsContextDependent = false,
        ),
        numericDesc = numeric.NumericDesc.plain.copy(
            literalBreakChar = numeric.BreakCharDesc.NoBreakChar,
            leadingDotAllowed = false,
            trailingDotAllowed = false,
            leadingZerosAllowed = true,
            positiveSign = numeric.PlusSignPresence.Optional,
            integerNumbersCanBeHexadecimal = true,

            //
            integerNumbersCanBeOctal = false, 

            integerNumbersCanBeBinary = false,
            realNumbersCanBeHexadecimal = false,
            realNumbersCanBeOctal = false,
            realNumbersCanBeBinary = false,
            hexadecimalLeads = Set('x', 'X'),
        ),
        textDesc = text.TextDesc.plain.copy(
            escapeSequences = text.EscapeDesc.plain.copy(
                escBegin = '\\',
            ),
            characterLiteralEnd = '\'',
            stringEnds = Set(("\"", "\"")),
            multiStringEnds = Set(("\"\"\"", "\"\"\"")),
            // graphicCharacter = "",
        ),

    )

    val keywords: Set[String] = Set(
        "int ", "bool ", "char ", "string ", "pair ", "null ", "skip", "read ", "free ", "return ", 
        "exit ", "print", "println", "if ", "then ", "else ", "fi", "while ", "do ", "is",
        "done", "begin", "end", "call", "fst ", "snd ", "newpair", "true", "false",
        ";" , "(", ")", "{", "}", "[", "]", ",",
        "!","-", "len", "ord", "chr", 
        "+", "-", "*", "/", "%","<", ">", "<=", ">=", "=", "==", "!=","&&", "||"
    )


    private val lexer = new Lexer(desc)

    val integer = lexer.lexeme.integer.decimal
    val implicits = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}



    
