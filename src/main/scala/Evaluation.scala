import scala.collection.mutable

object NFAEvaluator {
  def evaluate(nfa: State, input: String): Boolean = evaluate(Set(nfa), input)

  def evaluate(nfas: Set[State], input: String): Boolean = {
    input match {
      case "" => evaluateStates(nfas, None).exists(_ == Match())
      case string => evaluate(evaluateStates(nfas, input.headOption), string.tail)
    }
  }

  def evaluateStates(nfas: Set[State], input: Option[Char]): Set[State] = {
    val visitedStates = mutable.Set[State]()
    nfas.flatMap { state => evaluateState(state, input, visitedStates) }
  }

  def evaluateState(currentState: State, input: Option[Char], visitedStates: mutable.Set[State]): Set[State] = {
    if (visitedStates contains currentState) {
      Set()
    } else {
      visitedStates.add(currentState)
      currentState match {
        case placeholder: Placeholder => evaluateState(placeholder.pointingTo, input, visitedStates)
        case consume: Consume =>
          if (Some(consume.c) == input || consume.c == '.') {
            Set(consume.out)
          } else {
            Set()
          }
        case s: Split =>
          evaluateState(s.out1, input, visitedStates) ++
          evaluateState(s.out2, input, visitedStates)
        case m: Match =>
          if (input.isDefined) Set() else Set(Match())
      }
    }
  }
}