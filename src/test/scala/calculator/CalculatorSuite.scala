package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("testEval with a basic constant") {
    var namedExpressions: Map[String, Signal[Expr]] = Map() + ("a" -> Signal(Literal(2.0)))
    var basicExpression =  Literal(1.0)
    
    assert(Calculator.eval(basicExpression, namedExpressions) == 1.0)
  }
  
  test("testEval with a basic plus constants") {
    var namedExpressions: Map[String, Signal[Expr]] = Map() + ("a" -> Signal(Literal(2.0)))
    var aExprex   = Literal(1.0)
    var bExprex   = Literal(1.0)
    var sumExprex = Plus(aExprex, bExprex) 
    assert(Calculator.eval(sumExprex, namedExpressions) == 2.0)
  }
  
  test("testEval with a basic minus constants") {
    var namedExpressions: Map[String, Signal[Expr]] = Map() + ("a" -> Signal(Literal(2.0)))
    var aExprex   = Literal(3.0)
    var bExprex   = Literal(5.0)
    var sumExprex = Minus(aExprex, bExprex) 
    assert(Calculator.eval(sumExprex, namedExpressions) == -2.0)
  }
  
  test("testEval with a basic times constants") {
    var namedExpressions: Map[String, Signal[Expr]] = Map() + ("a" -> Signal(Literal(2.0)))
    var aExprex   = Literal(3.0)
    var bExprex   = Literal(5.0)
    var sumExprex = Times(aExprex, bExprex) 
    assert(Calculator.eval(sumExprex, namedExpressions) == 15.0)
  }
  
  test("testEval with a basic divide constants") {
    var namedExpressions: Map[String, Signal[Expr]] = Map() + ("a" -> Signal(Literal(2.0)))
    var aExprex   = Literal(15.0)
    var bExprex   = Literal(3.0)
    var sumExprex = Divide(aExprex, bExprex) 
    assert(Calculator.eval(sumExprex, namedExpressions) == 5.0)
  }
  
  test("testCalculatorCompute with only literals")
  {
    var nameExpressions: Map[String, Signal[Expr]] = Map() + ("a" -> Signal(Literal(1.0)), "b" -> Signal(Literal(1.0)))
    var computedValues = Calculator.computeValues(nameExpressions)
    assert(computedValues("a")() == 1.0)
    assert(computedValues("b")() == 1.0)
    
  }
  
  test("testCalculatorCompute with simple Plus and Minus literals")
  {
    var nameExpressions: Map[String, Signal[Expr]] = Map() + ("a" -> Signal(Plus(Literal(1.0),Literal(2.0))), 
                                                              "b" -> Signal(Minus(Literal(5.0),Literal(2.0))))
    var computedValues = Calculator.computeValues(nameExpressions)
    assert(computedValues("a")() == 3.0)
    assert(computedValues("b")() == 3.0)  
  }
  
  test("testCalculatorCompute with simple Plus and Minus literals and single Ref")
  {
    var nameExpressions: Map[String, Signal[Expr]] = Map() + ("a" -> Signal(Plus(Literal(1.0),Literal(2.0))), 
                                                              "b" -> Signal(Minus(Literal(5.0),Literal(2.0))),
                                                              "c" -> Signal(Ref("a"))
                                                             )
    var computedValues = Calculator.computeValues(nameExpressions)
    assert(computedValues("a")() == 3.0)
    assert(computedValues("b")() == 3.0)
    assert(computedValues("c")() == 3.0) 
  }
  
  test("should solve cyclic computations") {
    var namedExpressions: Map[String, Signal[Expr]] = Map() + ("a" -> Signal(Ref("b")))
    namedExpressions = namedExpressions + ("b" -> Signal(Ref("a")))
    // detects cyclic variables with the eval method
    assert(java.lang.Double.isNaN(Calculator.eval(Ref("a"), namedExpressions)))
    assert(java.lang.Double.isNaN(Calculator.eval(Ref("b"), namedExpressions)))

    // detects cyclic variables with the computeValues method
    var exprSignals = Calculator.computeValues(namedExpressions)
    val signalA = exprSignals.getOrElse("a", Var(0.0))
    assert(java.lang.Double.isNaN(signalA()))
    val signalB = exprSignals.getOrElse("b", Var(0.0))
    assert(java.lang.Double.isNaN(signalB()))
  }
  
}
