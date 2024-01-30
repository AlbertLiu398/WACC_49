package wacc

import parsley.Parsley
import parsley.combinator._
import parsley.token.{Lexer, predicate}
import parsley.token.descriptions._
import scala.collection.mutable.ListBuffer

object lexer {
    private val desc = LexicalDesc.plain.copy(
        // your configuration goes here
        nameDesc = NameDesc.plain.copy(
            identifierStart = predicate.Basic( c => c.isLetter | c == '_'),
            identifierLetter = predicate.Basic(c => c.isLetterOrDigit | c == '_'),
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
        ),

    )

    private val lexer = new Lexer(desc)

    val integer = lexer.lexeme.natural.number
    val floating = lexer.lexeme.floating.number
    val intOrFloat = lexer.lexeme.unsignedCombined.number
    val string = lexer.lexeme.string.ascii
    val graphicCharacter = lexer.lexeme.character.ascii
    val identifier = lexer.lexeme.names.identifier
    val implicits = lexer.lexeme.symbol.implicits
    def commaSep_[A](p: Parsley[A]): Parsley[List[A]] = lexer.lexeme.commaSep(p)
    // val escapeChar = choice(
    //     '0' #> '\u0000',
    //     'b' #> '\b',
    //     't' #> '\t',
    //     'n' #> '\n',
    //     'f' #> '\f',
    //     'r' #> '\r',
    //     '"' #> '\"',
    //     '\'',
    //     '\\'
    // )
    // val character = noneof('\\', '\', '"') <\> ("\\" *> escapeChar)

    //TODO : not compile yet
    // val newline: Lexeme[Unit] = lexer.lexeme(newline).void
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}




    
