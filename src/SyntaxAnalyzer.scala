import java.io.{FileOutputStream, ObjectOutputStream}
/*
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Instructor: Thyago Mota
 * Description: Prg01 - Syntax Analyzer
 * Student(s) Name(s): Hari Shrestha And Prashant Panth
 */

/* Grammar of the program is given below
program      = ´program´ identifier body ´.´
identifier   = letter { ( letter | digit ) }
body         = [ var_sct ] block
var_sct      = ´var´ var_dcl { ´;´ var_dcl }
var_dcl      = identifier { identifier } ´:´ type
type         = ´Integer´ | ´Boolean´
block        = ´begin´ stmt { ´;´ stmt } ´end´
stmt         = assgm_stmt | read_stmt | write_stmt | if_stmt | while_stmt | block
assgm_stmt   = identifier ´:=´ expr
read_stmt    = ´read´ identifier
write_stmt   = ´write´ ( identifier | literal )
if_stmt      = ´if´ bool_expr ´then´ stmt [ ´else´ stmt ]
while_stmt   = ´while´ bool_expr ´do´ stmt
expr         = arithm_expr | bool_expr
arithm_expr  = term arithm_expr'
arithm_expr'  = ( ´+´ | ´-´ ) term arithm_expr' | epsilon
term         = factor term'
term'         = ´*´ factor term' | epsilon
factor       = identifier | int_literal
literal      = int_literal | bool_literal
int_literal  = digit { digit }
bool_litreal = ´true´ | ´false´
bool_expr    = bool_literal | arithm_expr ( ´>´ | ´>=´ | ´=´ | ´<=´ | ´<´ ) arithm_expr
letter       = ´a´ | ´b´ | ´c´ | ´d´ | ´e´ | ´f´ | ´g´ | ´h´ | ´i´ | ´j´ | ´k´ | ´l´ | ´m´ | ´n´ | ´o´ | ´p´ | ´q´ | ´r´ | ´s´ | ´t´ | ´u´ | ´v´ | ´w´ | ´x´ | ´y´ | ´z´ | ´A´ | ´B´ | ´C´ | ´D´ | ´E´ | ´F´ | ´G´ | ´H´ | ´I´ | ´J´ | ´K´ | ´L´ | ´M´ | ´N´ | ´O´ | ´P´ | ´Q´ | ´R´ | ´S´ | ´T´ | ´U´ | ´V´ | ´W´ | ´X´ | ´Y´ | ´Z´
digit        = ´0´ | ´1´ | ´2´ | ´3´ | ´4´ | ´5´ | ´6´ | ´7´ | ´8´ | ´9´
*/


//SytaxAnalyzer begin here

class SyntaxAnalyzer(private var source: String) {

  private var it = new LexicalAnalyzer(source).iterator
  private var lexemeUnit: LexemeUnit = null

  private def getLexemeUnit() = {
    if (lexemeUnit == null) {
      lexemeUnit = it.next()
    }
  }

  //***************************************************************************************
  def parse(): Tree = {
    parseProgram()
  }

  //******************************************************************************************
  //program      = ´program´ identifier body ´.´
  private def parseProgram() = {
    val tree = new Tree("program")
    getLexemeUnit()
    if(lexemeUnit.getToken() != Token.EOF){
      if(lexemeUnit.getToken() == Token.PROGRAM) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
        tree.add(parseidentifier())
        tree.add(parsebody())
        if(lexemeUnit.getToken() == Token.PERIOD){
          tree.add(new Tree(lexemeUnit.getLexeme()))
          lexemeUnit = null
          getLexemeUnit()
        }
        else{
          throw new Exception("Syntax Analyzer Error: period expected!")
        }
        if(lexemeUnit.getToken() != Token.EOF){
          throw new Exception("Syntax Analyzer Error: EOF expected!")
        }

      }
    }

    tree
  }


  //*********************************************************************************************
  //identifier   = letter { ( letter | digit ) }
  private def parseidentifier() = {
     if (lexemeUnit.getToken() == Token.IDENTIFIER) {
       val tree = new Tree("identifier: '" + lexemeUnit.getLexeme() + "'")
       lexemeUnit = null
       getLexemeUnit()
       tree
     }
     else{
         throw new Exception("Syntax Analyzer Error: identifier expected!")
      }
  }


  //**************************************************************************************************
  //body  = [ var_sct ] block
  private def parsebody() = {
    val tree = new Tree("body")

    if(lexemeUnit.getToken() != Token.EOF){
      getLexemeUnit()
      if( lexemeUnit.getToken() == Token.VAR) {
        tree.add(parsevar_sct())
      }
      if( lexemeUnit.getToken() == Token.BEGIN_STMT) {
        tree.add(parseblock())
      }
      else{
        throw new Exception("Syntax Analyzer Error: begin expected!")
      }
    }
    tree
  }

  //********************************************************************************************
  //var_sct      = ´var´ var_dcl { ´;´ var_dcl }
  private def parsevar_sct() = {
    // create a tree with label "var_sct"
    val tree = new Tree("var_sct")
    //    getLexemeUnit()

    if(lexemeUnit.getToken() != Token.EOF){
      if(lexemeUnit.getToken() == Token.VAR) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
        tree.add(parsevar_dcl())
        var done = false
        while (!done) {
          if (lexemeUnit.getToken() == Token.SEMICOLON) {
            tree.add(new Tree(lexemeUnit.getLexeme()))
            lexemeUnit = null
            getLexemeUnit()
            tree.add(parsevar_dcl())
          }
          else
            done = true
        }
      }
      // else means "epsilon" production
    }
    // return the tree
    tree
  }

  //***************************************************************************************
  //var_dcl      = identifier { identifier } ´:´ type
  private def parsevar_dcl(): Tree = {
    //    println("parseSingleDefinition")
    val tree = new Tree("var_dct")

    var done = false
    while (!done) {
      tree.add(parseidentifier())
      if (lexemeUnit.getToken() == Token.IDENTIFIER) {
        done = false
      }else{
        done = true
      }
    }
    if (lexemeUnit.getToken() == Token.PUNCTUATOR) {
      tree.add(new Tree(lexemeUnit.getLexeme()))
      lexemeUnit = null
      getLexemeUnit()
      tree.add(parsetype())
    }
    else {
      throw new Exception("Syntax Analyzer Error: colon expected!")
    }
    tree
  }

  //******************************************************************************************
  //type         = ´Integer´ | ´Boolean´
  private def parsetype() = {
    // create a tree with label "type"
    val tree = new Tree("type")
    // TODOd: call getLexemeUnit

    // TODOd: if token is NOT EOF
    if (lexemeUnit.getToken() != Token.EOF) {
      if (lexemeUnit.getToken() == Token.INTEGER) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()

      }
      if (lexemeUnit.getToken() == Token.BOOLEAN) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
      }
    }

    // return the tree
    tree
  }


  //********************************************************************************************
  //block        = ´begin´ stmt { ´;´ stmt } ´end´
  private def parseblock() = {
    // create a tree with label "block"
    val tree = new Tree("block")
    //    getLexemeUnit()

    if(lexemeUnit.getToken() != Token.EOF){


      if(lexemeUnit.getToken() == Token.BEGIN_STMT) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
        tree.add(parsestmt())
        var done = false
        while (!done) {
          if (lexemeUnit.getToken() == Token.SEMICOLON) {
            tree.add(new Tree(lexemeUnit.getLexeme()))
            lexemeUnit = null
            getLexemeUnit()
            tree.add(parsestmt())
          }
          else
            done = true

        }

      }

      if(lexemeUnit.getToken() == Token.END) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
      }
      else{
        throw new Exception("Syntax Analyzer Error: end expected!")
      }

    }

    // return the tree
    tree
  }


  //********************************************************************************************
  //stmt         = assgm_stmt | read_stmt | write_stmt | if_stmt | while_stmt | block
  private def parsestmt(): Tree = {
    val tree = new Tree("stmt")
    if(lexemeUnit.getToken() != Token.EOF) {
      if (lexemeUnit.getToken() == Token.IDENTIFIER) {
        tree.add(parseassgm_stmt())
      }
      if (lexemeUnit.getToken() == Token.READ_STMT) {
        tree.add(parseread_stmt())
      }
      if (lexemeUnit.getToken() == Token.WRITE_STMT) {
        tree.add(parsewrite_stmt())
      }
      if (lexemeUnit.getToken() == Token.IF) {
        tree.add(parseif_stmt())
      }
      if (lexemeUnit.getToken() == Token.WHILE) {
        tree.add(parsewhile_stmt())
      }
      if (lexemeUnit.getToken() == Token.BEGIN_STMT) {
        tree.add(parseblock())
      }
    }

    tree
  }

  //************************************************************************************
  //assgm_stmt   = identifier ´:=´ expr
  private def parseassgm_stmt() = {
    // create a tree with label "assgm_stmt"
    val tree = new Tree("assgm_stmt")
    if(lexemeUnit.getToken() != Token.EOF) {

      if (lexemeUnit.getToken() == Token.IDENTIFIER) {
        tree.add(parseidentifier())
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()

        tree.add(parseexpr())

      }

    }

    // return the tree
    tree
  }


  //*****************************************************************************************
  //read_stmt    = ´read´ identifier
  private def parseread_stmt() = {
    // create a tree with label "read_stmt"
    val tree = new Tree("read_stmt")

    if(lexemeUnit.getToken() != Token.EOF){

      if(lexemeUnit.getToken() == Token.READ_STMT) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
        tree.add(parseidentifier())

      }
    }

    // return the tree
    tree
  }

  //****************************************************************************************
  //write_stmt   = ´write´ ( identifier | literal )
  private def parsewrite_stmt() = {
    // create a tree with label "write_stmt"
    val tree = new Tree("write_stmt")
    if(lexemeUnit.getToken() != Token.EOF) {

      if (lexemeUnit.getToken() == Token.WRITE_STMT) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
        if (lexemeUnit.getToken() == Token.IDENTIFIER) {
          tree.add(parseidentifier())
        }
        if (lexemeUnit.getToken() == Token.LITERAL){
          tree.add(parseliteral())
        }

      }

    }
    // return the tree
    tree
  }

  //***************************************************************************************
  //if_stmt      = ´if´ bool_expr ´then´ stmt [ ´else´ stmt ]
  private def parseif_stmt() = {
    // create a tree with label "if_stmt"
    val tree = new Tree("if_stmt")
    if(lexemeUnit.getToken() != Token.EOF) {

      if (lexemeUnit.getToken() == Token.IF) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
        tree.add(parsebool_expr())
      }
      if (lexemeUnit.getToken() == Token.THEN) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
        tree.add(parsestmt())
        tree.add(parsebool_expr())

      }
      else{
        throw new Exception("Syntax Analyzer Error: then expected!")
      }
      if (lexemeUnit.getToken() == Token.ELSE) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
        tree.add(parsestmt())

      }


    }
    // return the tree
    tree
  }


  //*****************************************************************************************
  //while_stmt   = ´while´ bool_expr ´do´ stmt
  private def parsewhile_stmt() = {
    // create a tree with label "while_stmt"
    val tree = new Tree("while_stmt")
    if(lexemeUnit.getToken() != Token.EOF) {

      if (lexemeUnit.getToken() == Token.WHILE) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
        tree.add(parsebool_expr())
      }
      if (lexemeUnit.getToken() == Token.DO) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
        tree.add(parsestmt())
      }
      else {
        throw new Exception("Syntax Analyzer Error: do expected!")
      }
    }
    tree
  }


  //*******************************************************************************************************
  //expr         = arithm_expr | bool_expr
  private def parseexpr() = {
    // create a tree with label "expr"
    val tree = new Tree("expr")
    if (lexemeUnit.getToken() != Token.EOF) {
      if (lexemeUnit.getToken() == Token.INT_LITERAL || lexemeUnit.getToken() == Token.IDENTIFIER ) {
        tree.add(parsearithm_expr())

      }
      if (lexemeUnit.getToken() == Token.BOOL_LITERAL) {
          tree.add(parsebool_expr())

      }

    }
    // return the tree
    tree
  }

  //*****************************************************************************************************
  //arithm_expr  = term arithm_expr'
  private def parsearithm_expr() = {
    // create a tree with label "arithm_expr"
    val tree = new Tree("arithm_expr")
    if (lexemeUnit.getToken() != Token.EOF) {
      tree.add(parseterm())
      tree.add(parsearithm_exprprime())
    }
    // return the tree
    tree
  }

  //**********************************************************************************************************
  //arithm_expr'  = ( ´+´ | ´-´ ) term arithm_expr' | epsilon
  private def parsearithm_exprprime():Tree= {
    // create a tree with label "arithm_expr'"
    val tree = new Tree("aritm_expr'")
    // TODOd: if token is NOT EOF
    if (lexemeUnit.getToken() != Token.EOF) {

      if (lexemeUnit.getToken() == Token.ADD_OP || lexemeUnit.getToken() == Token.SUB_OP) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
        tree.add(parseterm())
        tree.add(parsearithm_exprprime())
      }
      // else means "epsilon" production
    }
    // return the tree
    tree
  }

  //**********************************************************************************************************
  //term = factor term'
  private def parseterm() = {
    // create a tree with label "term"
    val tree = new Tree("term")
    if (lexemeUnit.getToken() != Token.EOF) {
      tree.add(parsefactor())
      tree.add(parsetermprime())
    }
    // return the tree
    tree
  }



  //**********************************************************************************************************
  //term'= ´*´ factor term' | epsilon
  private def parsetermprime():Tree = {
    // create a tree with label "term'"
    val tree = new Tree("term'")
    // TODOd: if token is NOT EOF
    if (lexemeUnit.getToken() != Token.EOF) {
      if (lexemeUnit.getToken() == Token.MUL_OP) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
        tree.add(parsefactor())
        tree.add(parsetermprime())

      }
      // else means "epsilon" production
    }
    // return the tree
    tree
  }


  //**********************************************************************************************************
  //factor = identifier | int_literal
  private def parsefactor() = {
    // create a tree with label "factor"
    val tree = new Tree("factor")
    //    tree.add(new Tree(lexemeUnit.getLexeme()))
    if (lexemeUnit.getToken() != Token.EOF) {
      if (lexemeUnit.getToken() == Token.IDENTIFIER) {
        tree.add(parseidentifier())

      }
      if (lexemeUnit.getToken() == Token.INT_LITERAL) {
        tree.add(parseint_literal())
      }
    }

    tree
  }

  //**********************************************************************************************************
  //literal      = int_literal | bool_literal
  private def parseliteral() = {
    val tree = new Tree("literal")
    if (lexemeUnit.getToken() != Token.EOF) {
      if (lexemeUnit.getToken() == Token.INT_LITERAL) {
        tree.add(parseint_literal())
      }
      else if (lexemeUnit.getToken() == Token.BOOL_LITERAL) {
        tree.add(parsebool_literal())
      }
    }
    // return the tree
    tree
  }

  //**********************************************************************************************************
  //int_literal  = digit { digit }
  private def parseint_literal() = {
    // create a tree with label "int_literal"
    val tree = new Tree("int_literal: '" + lexemeUnit.getLexeme() + "'")
    lexemeUnit = null
    getLexemeUnit()

    tree
  }


  //**********************************************************************************************************
  //bool_litreal = ´true´ | ´false´
  private def parsebool_literal() = {
    // create a tree with label "bool_literal"
    val tree = new Tree("bool_literal")
    if (lexemeUnit.getToken() != Token.EOF) {
      if (lexemeUnit.getToken() == Token.BOOL_LITERAL) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()

      }

    }
    // return the tree
    tree
  }


  //**********************************************************************************************************
  //bool_expr    = bool_literal | arithm_expr ( ´>´ | ´>=´ | ´=´ | ´<=´ | ´<´ ) arithm_expr
  private def parsebool_expr() = {
    // create a tree with label "bool_expr"
    val tree = new Tree("bool_expr")

    if (lexemeUnit.getToken() != Token.EOF) {
      if (lexemeUnit.getToken() == Token.BOOL_LITERAL) {
        tree.add(parsebool_literal())

      }
      if (lexemeUnit.getToken() == Token.IDENTIFIER) {
        tree.add(parsearithm_expr())
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
      }

      if (lexemeUnit.getToken() == Token.IDENTIFIER) {
        tree.add(parsearithm_expr())
      }

    }

    // return the tree
    tree
  }
}


//**********************************************************************************************************
object SyntaxAnalyzer {
  def main(args: Array[String]): Unit = {
    // check if source file was passed through the command-line
    if (args.length != 1) {
      print("Missing source file!")
      System.exit(1)
    }

    val syntaxAnalyzer = new SyntaxAnalyzer(args(0))
    val parseTree = syntaxAnalyzer.parse()
    print(parseTree)

  }
} //End of SyntaxAnalyzer

//**********************************************************************************************************