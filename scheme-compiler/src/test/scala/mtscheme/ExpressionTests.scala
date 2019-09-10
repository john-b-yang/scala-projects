package mtscheme

import org.scalatest.FunSuite
import mtscheme.Interpreter._
import mtscheme.Parser._
import mtscheme.BuiltIn._

class ExpressionTests extends FunSuite {
  def getNumResult(env: Env, expr: ExprT) = eval(env, expr)._2 match {
    case Value(Num(n))  => n
    case _              => throw new IllegalArgumentException("num expression failure")
  }

  def testNumber(env: Env, exprS: String, correct: BigDecimal) = {
    expectResult(correct) { getNumResult(env, parse(exprS).head) }
  }

  def testNumberG = (testNumber _).curried(globalEnv)

  test("add") {
    testNumberG("(+ 1 2)")        (1+2)
    testNumberG("(+ 1 (+ 2 3))")  (1+2+3)
    testNumberG("(+ 7)")          (7)
    testNumberG("(+ 1 1 1 0.14)") (1+1+1+0.14)
  }

  test("sub") {
    testNumberG("(- 1 2)")        (1-2)
    testNumberG("(- 1 (- 2 3))")  (1-(2-3))
    testNumberG("(- 1 1 1)")      (1-1-1)
  }
}
