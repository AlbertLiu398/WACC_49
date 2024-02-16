

package wacc

import parsley.Parsley
import parsley.combinator._
import parsley.token.{Lexer, predicate}
import parsley.token.descriptions._
import scala.collection.mutable.ListBuffer
import parsley.token.errors._
import parsley.token.numeric._
import parsley.Parsley._
import parsley.character._
import parsley.debug, debug._ 



object lexer {
    // lexer configuration : 
    private val desc = LexicalDesc.plain.copy(

        nameDesc = NameDesc.plain.copy(
            identifierStart = predicate.Basic( c => c.isLetter | c == '_'),
            identifierLetter = predicate.Basic(c => c.isLetterOrDigit | c == '_'),
        ),

        symbolDesc = SymbolDesc.plain.copy(
            caseSensitive = false,
            hardKeywords = Set("null", "skip", "read", "free", "return", "exit", "print", "println", 
            "if", "then", "else", "fi", "while", "do", "is","done", "begin", "end", "call", "fst ", 
            "snd", "newpair", "true", "false", "int", "bool", "char", "string", "pair", "array", "len", "ord", "chr"),
            hardOperators = Set("!","-", 
            "+", "-", "*", "/", "%","<", ">", "<=", ">=", "==", "!=","&&", "||"),
        ),

        spaceDesc = SpaceDesc.plain.copy(
            lineCommentStart = "#",
            lineCommentAllowsEOF = true,
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
                escBegin = '\\' ,
                literals = Set('\"', '\'', '\\'),   
                mapping = Map(
                    "0" -> 0x0000
                    , "t" -> 0x0009
                    , "b" -> 0x0008
                    , "n" -> 0x000a
                    , "r" -> 0x000d
                    , "f"-> 0x000c
                ),
            ),
            characterLiteralEnd = '\'',
            stringEnds = Set(("\"", "\"")),
            multiStringEnds = Set(("\"\"\"", "\"\"\"")),
            graphicCharacter = predicate.Basic(c => c != '\"' && c != '\\' && c!= '\'' && c >= ' '),
        ),
    )
    // configuration for error messages
      val errConfig = new ErrorConfig {
        override def labelSymbol = Map(
            "++" -> LabelAndReason(
                reason = "unexpected ++, array literals are not first-class expressions",
                label = "++",
            ),

            ">" -> LabelAndReason(
                reason = "unclosed angle bracket",
                label = "closing angle bracket",
            ),
            "}" -> LabelAndReason(
                reason = "unclosed brace",
                label = "closing brace",
            ),
            ")" -> LabelAndReason(
                reason = "unclosed parenthesis",
                label = "closing parenthesis",
            ),
            "]" -> LabelAndReason(
                reason = "unclosed square bracket",
                label = "closing square bracket",
            ),
            ":" -> LabelAndReason(
                reason = "unexpected colon",
                label = "colon",
            ),
            "," -> LabelAndReason(
                reason = "unexpected comma",
                label = "comma",
            ),
            "." -> LabelAndReason(
                reason = "unexpected dot",
                label = "dot",
            ),
            "<" -> LabelAndReason(
                reason = "unexpected open angle bracket",
                label = "open angle bracket",
            ),
            "{" -> LabelAndReason(
                reason = "unexpected open brace",
                label = "open brace",
            ),
            "(" -> LabelAndReason(
                reason = "unexpected (",
                label = "open parenthesis",
            ),

            "[" -> LabelAndReason(
                reason = "unexpected open square bracket",
                label = "open square bracket",
            ),
            
            ";" -> LabelAndReason(
                reason = "unexpected semicolon",
                label = "semicolon",
            ) 
        )
    }

    // lexer instance used in parser 
    private val lexer = new Lexer(desc,errConfig)
    val integer = lexer.lexeme.integer.decimal32
    val floating = lexer.lexeme.floating.number
    val intOrFloat = lexer.lexeme.unsignedCombined.number
    val string = lexer.lexeme.string.ascii
    val graphicCharacter = lexer.lexeme.character.ascii
    val identifier = lexer.lexeme.names.identifier
    val implicits = lexer.lexeme.symbol.implicits
    def commaSep1_[A](p: Parsley[A]): Parsley[List[A]] = lexer.lexeme.commaSep1(p)
    def commaSep_[A](p: Parsley[A]): Parsley[List[A]] = lexer.lexeme.commaSep(p)

    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
