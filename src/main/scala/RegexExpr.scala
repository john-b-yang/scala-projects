/*
 * Part 1: Define the hierarchy classes for constructing an abstract syntax tree.
 */
abstract class RegexExpr
case class Literal(c: Char) extends RegexExpr // '.', 'a'
case class Or(expr1: RegexExpr, expr2: RegexExpr) extends RegexExpr // a|b
case class Concat(first: RegexExpr, second: RegexExpr) extends RegexExpr // ab => Concat(a,b); abc => Concat(a, Concat(b, c))
case class Repeat(expr: RegexExpr) extends RegexExpr
case class Plus(expr: RegexExpr) extends RegexExpr
