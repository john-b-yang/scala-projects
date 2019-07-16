/*
 * Part 2: Parsing - Converting from a regex string to a tree representation (Combinator Library)
 *
 * With Scala's parser combinator library, we can directly write rules describing our language
 * Order of Operations: Operators "bind" characters with different strengths, so we parse the
 * weakest operators, represented earlier in the tree, then stronger operators closer to the leaves.
 */

/*
 * Bind Strength Order (Strongest to Weakest):
 * 1. Character Literal + Parentheses
 * 2. + and *
 * 3. Concatenation
 * 4. | (or)
 *
 * 4 Levels of Binding Strength = 4 Different Types of Expressions
 */

import scala.util.parsing.combinator._

object RegexParser extends RegexParsers {
  /* Lowest Level Bindings */
  // Deciphered:
  // Right hand side: If something matches a word (\w) or period, turn it into a literal
  def charLiteral: Parser[RegexExpr] = ("""\w""".r | ".") ^^ { char => Literal(char.head) }
  def parenExpr: Parser[RegexExpr] = "(" ~> highExpr <~ ")"
  def literal: Parser[RegexExpr] = charLiteral | parenExpr

  /* + and * bindings */
  def repeat: Parser[RegexExpr] = literal <~ "*" ^^ { case l => Repeat(l) } // *
  def plus: Parser[RegexExpr] = literal <~ "+" ^^ { case p => Plus(p) } // +
  def lowExpr: Parser[RegexExpr] = repeat | plus | literal

  /* Concatenation bindings */
  def concat: Parser[RegexExpr] = rep(lowExpr) ^^ { case list => listToConcat(list)}
  def midExpr: Parser[RegexExpr] = concat | lowExpr

  /* Or (|) Bindings */
  def or: Parser[RegexExpr] = midExpr ~ "|" ~ midExpr ^^ { case l ~ "|" ~ r => Or(l, r)}
  def highExpr: Parser[RegexExpr] = or | midExpr

  /* Helper Functions */
  def listToConcat(list: List[RegexExpr]): RegexExpr = list match {
    case head :: Nil => head
    case head :: rest => Concat(head, listToConcat(rest))
  }

  def apply(input: String): Option[RegexExpr] = parseAll(highExpr, input) match {
    case Success(result, _) => Some(result)
    case failure: NoSuccess => None
  }
}
