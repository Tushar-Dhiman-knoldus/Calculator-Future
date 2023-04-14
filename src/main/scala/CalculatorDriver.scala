package com.knoldus.calculator

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object CalculatorDriver extends App {

  val calculator = Calculator

  // Add
  private val addResult = calculator.calculate("+", Seq(2.0, 3.0))
  addResult.onComplete {
    case Success(value) => println(s"Add Result: $value")
    case Failure(exception) => println(s"Exception: ${exception.getMessage}")
  }

  // Subtract
  private val subtractResult = calculator.calculate("-",Seq(5.0, 3.0))
  subtractResult.onComplete {
    case Success(value) => println(s"Subtract Result: $value")
    case Failure(exception) => println(s"Exception: ${exception.getMessage}")
  }

  // Multiply
   private val multiplyResult = calculator.calculate("*", Seq(2.0, 3.0))
  multiplyResult.onComplete {
    case Success(value) => println(s"Multiply Result: $value")
    case Failure(exception) => println(s"Exception: ${exception.getMessage}")
  }

  // Divide
  private val divideResult = calculator.calculate("/",Seq(6.0, 3.0))
  divideResult.onComplete {
    case Success(value) => println(s"Divide Result: $value")
    case Failure(exception) => println(s"Exception: ${exception.getMessage}")
  }

  // Power
  private val powerResult = calculator.calculate("^",Seq(2.0, 4.0))
  powerResult.onComplete {
    case Success(value) => println(s"Power Result: $value")
    case Failure(exception) => println(s"Exception: ${exception.getMessage}")
  }

  // Square root
  private val sqrtResult = calculator.calculate("sqrt",Seq(9))
  sqrtResult.onComplete {
    case Success(value) => println(s"Square Root Result: $value")
    case Failure(exception) => println(s"Exception: ${exception.getMessage}")
  }

  // Factorial
  private val factorialResult = calculator.calculate("!",Seq(5))
  factorialResult.onComplete {
    case Success(value) => println(s"Factorial Result: $value")
    case Failure(exception) => println(s"Exception: ${exception.getMessage}")
  }

  // Sum
  private val sumResult = calculator.calculate("sum",Seq(1, 2, 3, 4, 5))
  sumResult.onComplete {
    case Success(value) => println(s"Sum Result: $value")
    case Failure(exception) => println(s"Exception: ${exception.getMessage}")
  }

  // GCD
  private val gcdResult = calculator.calculate("gcd",Seq(20, 30))
  gcdResult.onComplete {
    case Success(value) => println(s"GCD Result: $value")
    case Failure(exception) => println(s"Exception: ${exception.getMessage}")
  }

  // Odd Numbers
  private val oddNumbersResult = calculator.calculate("odd",Seq(1, 2, 3, 4, 5))
  oddNumbersResult.onComplete {
    case Success(value) => println(s"Odd Numbers Result: $value")
    case Failure(exception) => println(s"Exception: ${exception.getMessage}")
  }

  // Even Numbers
  private val evenNumbersResult = calculator.calculate("even",List(1, 2, 3, 4, 5))
  evenNumbersResult.onComplete {
    case Success(value) => println(s"Even Numbers Result: $value")
    case Failure(exception) => println(s"Exception: ${exception.getMessage}")
  }

  // Fibonacci
  private val fibonacciResult = calculator.calculate("fibonacci",Seq(5))
  fibonacciResult.onComplete {
    case Failure(exception) => println(s"Exception: ${exception.getMessage}")
    case Success(value) => println(s"Fibonacci Result: $value")
  }


  // Instance for other three method that is used to perform multiple operation.
  private val expressionEvaluator = ExpressionEvaluator

  // Here I am calling the Square of Expression to perform operations
  private val squareOfExpression = expressionEvaluator.squareOfExpression(12.0, 14.0)
  println("Is Expression Equal :" + squareOfExpression)

  // calling the find method to find the factorial number greater than 6^n
  private val findNumbers = expressionEvaluator.find(Seq(100.0, 200.0, 10.0))
  findNumbers.onComplete {
    case Success(value) => println("Factorial Greater than 6^n :" + value)
    case Failure(exception) => println("Error", exception)
  }

  // calling the find Average method to perform the operations
  private val averageAfterChaining = expressionEvaluator.findAverageAfterChainingOperations(Seq(4.0, 3.0, 5.0, 10.0))
  averageAfterChaining.onComplete {
    case Success(value) => println(s"$value")
    case Failure(exception) => println("Error", exception)
  }
  // Wait for all the futures to complete
  Thread.sleep(1000)
}

