/*
 * Part 3: Parsing - Converting the Abstract Syntax Tree into a walk-able, Nondeterministic Finite Automata (NFA)
 *
 * Each operation maps to a specific component in an NFA. The expressions we're including can be encapsulated
 * by Consume, Split, and Match. We use case classes for match b/c it provides "value based equality"
 */

abstract class State

class Consume(val c: Char, val out: State) extends State
class Placeholder(var pointingTo: State) extends State
class Split(val out1: State, val out2: State) extends State
case class Match() extends State

object  NFA {
  def regexToNFA(regex: RegexExpr): State = regexToNFA(regex, Match())

  private def regexToNFA(regexExpr: RegexExpr, andThen: State): State = {
    regexExpr match {
      case Literal(c) => new Consume(c, andThen)
      case Concat(first, second) => {
        regexToNFA(first, regexToNFA(second, andThen))
      }
      case Or(l, r) => new Split(
        regexToNFA(l, andThen),
        regexToNFA(r, andThen)
      )
      case Repeat(r) =>
        val placeholder = new Placeholder(null)
        val split = new Split(regexToNFA(r, placeholder), andThen)
        placeholder.pointingTo = split
        placeholder
      case Plus(r) => regexToNFA(Concat(r, Repeat(r)), andThen)
    }
  }
}