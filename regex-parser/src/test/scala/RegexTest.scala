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

    "match more complex regexes" in {
      Regex.fullMatch("a", "(a*)*") should be(true)
      Regex.fullMatch("b", "(a*)*") should be(false)
      Regex.fullMatch("abc", "abc") should be(true)

      Regex.fullMatch("abc", "(a|b)bc") should be(true)
      Regex.fullMatch("abc", "(a|b)+bc") should be(true)
      Regex.fullMatch("abc", "(a+b)+b*c") should be(true)
      Regex.fullMatch("abc", "((a|b)+b*c)+") should be(true)
    }

    "match this pattern a million times" in {
      (1 to 10000).foreach(_ => Regex.fullMatch("abcabcabcabcabcabcabcd", "((a|b)+b*c)+"))
    }
  }
}
