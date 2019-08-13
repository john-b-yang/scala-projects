import scala.io.StdIn.readLine

object Regex {
  def fullMatch(input: String, pattern: String) = {
    val parsed = RegexParser(pattern).getOrElse( throw new RuntimeException("Failed to parse regex") )
    val nfa = NFA.regexToNFA(parsed)
    NFAEvaluator.evaluate(nfa, input)
  }

  def matchAnywhere(input: String, pattern: String) = fullMatch(input, ".*" + pattern + ".*")

  def main(args: Array[String]): Unit = {
    val regex = readLine("Input regex pattern > ")
    val string = readLine("Input string to match > ")
    println(s"Full Match? - ${Regex.fullMatch(string, regex)}")
    println(s"Partial Match? - ${Regex.matchAnywhere(string, regex)}")
  }
}