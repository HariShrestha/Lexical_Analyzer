//import LexicalAnalyzer.{OPERATOR_PUNCTUATOR_TO_TOKEN, WORD_TO_TOKEN}

import scala.io.Source

/*
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Instructor: Thyago Mota
 * Description: Prg01 - Lexical Analyzer
 * Student(s) Name(s): Prashant Panth and Hari Shrestha
 */

//*********************************************************************************************************
class LexicalAnalyzer(private var source: String) extends Iterable[LexemeUnit] {

  private var input = ""
  for (line <- Source.fromFile(source).getLines)
    input += line + "\n"
  // determines the class of a given character
  private def getCharClass(c: Char): CharClass.Value = {
    if (LexicalAnalyzer.LETTERS.contains(c))
      CharClass.LETTER
    else if (LexicalAnalyzer.DIGITS.contains(c))
      CharClass.DIGIT
    else if (LexicalAnalyzer.BLANKS.contains(c))
      CharClass.BLANK
    else if (c == '+' || c == '-' || c == '*' || c == '/')
      CharClass.OPERATOR
    else if (c == '(' || c == ')' || c == '[' || c == ']' || c == '{' || c == '}')
      CharClass.DELIMITER
    else
      CharClass.OTHER
  }

  // reads the input until a non-blank character is found, returning the input updated
  private def readBlanks: Unit = {
    var foundNonBlank = false
    while (input.length > 0 && !foundNonBlank) {
      val c = input(0)
      if (getCharClass(c) == CharClass.BLANK)
        input = input.substring(1)
      else
        foundNonBlank = true
    }
  }

  //*********************************************************************************************************
  def iterator: Iterator[LexemeUnit] = {
    new Iterator[LexemeUnit] {

      override def hasNext: Boolean = {
        readBlanks
        input.length > 0
      }

      override def next(): LexemeUnit = {
        if (!hasNext)
          new LexemeUnit("", Token.EOF)
        else {
          var lexeme = ""
          readBlanks
          if (input.length == 0)
            new LexemeUnit(lexeme, Token.EOF)
          else {
            var c = input(0)
            var charClass = getCharClass(c)



            // Done: recognize a letter followed by letters (or digits) as an identifier
            if (charClass == CharClass.LETTER) {
              input = input.substring(1)
              lexeme += c
              var noMoreLetterDigits = false
              while (!noMoreLetterDigits) {
                if (input.length == 0)
                  noMoreLetterDigits = true
                else {
                  c = input(0)
                  charClass = getCharClass(c)
                  if (charClass == CharClass.LETTER || charClass == CharClass.DIGIT) {
                    input = input.substring(1)
                    lexeme += c
                  }
                  else
                    noMoreLetterDigits = true
                }
              }

              lexeme match {
                case "read" => return new LexemeUnit(lexeme, Token.READ_STMT)
                case "write" => return new LexemeUnit(lexeme, Token.WRITE_STMT)
                case "begin" => return new LexemeUnit(lexeme, Token.BEGIN_STMT)
                case "program" => return new LexemeUnit(lexeme, Token.PROGRAM)
                case "Integer" => return new LexemeUnit(lexeme, Token.INTEGER)
                case "var" => return new LexemeUnit(lexeme, Token.VAR)
                case "while" => return new LexemeUnit(lexeme, Token.WHILE)
                case "end" => return new LexemeUnit(lexeme, Token.END)
                case "Boolean" => return new LexemeUnit(lexeme, Token.BOOLEAN)
                case "if" => return new LexemeUnit(lexeme, Token.IF)
                case "true" => return new LexemeUnit(lexeme, Token.BOOL_LITERAL)
                case "false" => return new LexemeUnit(lexeme, Token.BOOL_LITERAL)
                case "do" => return new LexemeUnit(lexeme, Token.DO)
                case "then" => return new LexemeUnit(lexeme, Token.THEN)
                case "else" => return new LexemeUnit(lexeme, Token.ELSE)
                case _  => return new LexemeUnit(lexeme, Token.IDENTIFIER)
              }
            }


            // Done: recognize multiple digits as a literal
            if (charClass == CharClass.DIGIT) {
              input = input.substring(1)
              lexeme += c
              var noMoreDigits = false
              while (!noMoreDigits) {
                if (input.length == 0)
                  noMoreDigits = true
                else {
                  c = input(0)
                  charClass = getCharClass(c)
                  if (charClass == CharClass.DIGIT) {
                    input = input.substring(1)
                    lexeme += c
                  }
                  else
                    noMoreDigits = true
                }
              }
              return new LexemeUnit(lexeme, Token.INT_LITERAL)
            }
            // recognize punctators
            if (c == ';' || c == '.') {
              input = input.substring(1)
              lexeme += c
              c match {
                case ';' => return new LexemeUnit(lexeme, Token.SEMICOLON)
                case '.' => return new LexemeUnit(lexeme, Token.PERIOD)

              }

            }


            // Done: recognize punctuations
            if (c == ':') {
              input = input.substring(1)
              lexeme += c
              var noMoreLetterDigits = false
              while (!noMoreLetterDigits) {
                if (input.length == 0)
                  noMoreLetterDigits = true
                else {
                  c = input(0)
                  //                  charClass = getCharClass(c)
                  if (c == '=') {
                    input = input.substring(1)
                    lexeme += c
                  }
                  else
                    noMoreLetterDigits = true
                }
              }
              return new LexemeUnit(lexeme, Token.PUNCTUATOR)
            }

            // Done: recognize airthmetic_expr
            if (c == '<' || c == '>') {
              input = input.substring(1)
              lexeme += c
              var noMoreLetterDigits = false
              while (!noMoreLetterDigits) {
                if (input.length == 0)
                  noMoreLetterDigits = true
                else {
                  c = input(0)
                  //                  charClass = getCharClass(c)
                  if (c == '=') {
                    input = input.substring(1)
                    lexeme += c
                  }
                  else
                    noMoreLetterDigits = true
                }
              }
              return new LexemeUnit(lexeme, Token.ARITHM_EXPR)
            }

            // Done: recognize operators
            if (charClass == CharClass.OPERATOR) {
              input = input.substring(1)
              lexeme += c
              c match {
                case '+' => return new LexemeUnit(lexeme, Token.ADD_OP)
                case '-' => return new LexemeUnit(lexeme, Token.SUB_OP)
                case '*' => return new LexemeUnit(lexeme, Token.MUL_OP)
                case '/' => return new LexemeUnit(lexeme, Token.DIV_OP)
              }
            }
            // Done: recognize multiple digits as a literal
            if (charClass == CharClass.DIGIT) {
              input = input.substring(1)
              lexeme += c
              var noMoreDigits = false
              while (!noMoreDigits) {
                if (input.length == 0)
                  noMoreDigits = true
                else {
                  c = input(0)
                  charClass = getCharClass(c)
                  if (charClass == CharClass.DIGIT) {
                    input = input.substring(1)
                    lexeme += c
                  }
                  else
                    noMoreDigits = true
                }
              }
              return new LexemeUnit(lexeme, Token.INT_LITERAL)
            }

            // throw an exception if an unrecognizable symbol is found
            throw new Exception("Lexical Analyzer Error: unrecognizable symbol found!")
          }
        }
      } // end next
    } // end 'new' iterator
  } // end iterator method
} // end LexicalAnalyzer class

//*********************************************************************************************************

object LexicalAnalyzer {
  val LETTERS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val DIGITS  = "0123456789"
  val BLANKS  = " \n\t"
  val WORD_TO_TOKEN = Map(
    "read"  -> Token.READ_STMT,
    "being"     -> Token.BEGIN_STMT,
    "write"  -> Token.WRITE_STMT
  )

  def main(args: Array[String]): Unit = {
    // check if source file was passed through the command-line
    if (args.length != 1) {
      print("Missing source file!")
      System.exit(1)
    }

    val lex = new LexicalAnalyzer(args(0))
    val it = lex.iterator
    while (it.hasNext) {
      val lexemeUnit = it.next()
      println(lexemeUnit)
    }
  } // end main method
} // end LexicalAnalyzer object

//*********************************************************************************************************