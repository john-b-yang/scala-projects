import org.scalatest._

class SchemeTest extends WordSpec with Matchers {
  "A Scheme Compiler" should {
    "Perform basic arithmetic" in {
      (1 + 1) should be(2)
    }
  }
}
