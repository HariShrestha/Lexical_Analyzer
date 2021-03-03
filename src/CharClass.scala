/*
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Instructor: Thyago Mota
 * Description: Prg01 - CharClass
 * Student(s) Name(s):
 */

object CharClass extends Enumeration {
  val EOF        = Value
  val LETTER     = Value
  val DIGIT      = Value
  val OPERATOR   = Value
  val PUNCTUATOR = Value
  val DELIMITER  = Value
  val QUOTE      = Value
  val BLANK      = Value
  val OTHER      = Value
}
