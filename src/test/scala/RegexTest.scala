import org.scalatest._

class RegexTest extends WordSpec with Matchers {
  "A regex evaluator" should {
    "match these basic regexes" in {
      Regex.fullMatch("ab", "ab") should be(true)
      Regex.fullMatch("abbbbb", "ab+") should be(true)
      Regex.fullMatch("bbbbb", "ab+") should be(false)
      Regex.fullMatch("aaabbb", "a+b+") should be(true)
      Regex.fullMatch("ababa", "a+b+") should be(false)
      Regex.fullMatch("aaa", "a*") should be(true)
      Regex.fullMatch("", "a*") should be(true)
    }

    "match the . character properly" in {
      Regex.fullMatch("a", ".") should be(true)
      Regex.fullMatch("ab", ".") should be(false)
      Regex.fullMatch("abab", "(..)*") should be(true)
      Regex.fullMatch("aba", "(..)*") should be(false)
    }
  }
}
