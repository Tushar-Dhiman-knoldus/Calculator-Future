package com.knoldus.calculator

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

// Adding the numbers.
object AddTheNumbers extends ValidateOperator {

  override def validate(operands: Seq[Double]): Boolean = {
    if (operands.size == 2) true
    else false
  }

  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    Seq(operands.head + operands.last)
  }
}

// Subtracting the numbers.
object SubtractTheNumbers extends ValidateOperator {

  override def validate(operands: Seq[Double]): Boolean = {
    if (operands.size == 2) true
    else false
  }

  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    Seq(operands.head - operands.last)
  }
}

// Multiplying the numbers.
object MultiplyTheNumbers extends ValidateOperator {

  override def validate(operands: Seq[Double]): Boolean = {
    if (operands.size == 2) true
    else false
  }

  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    Seq(operands.head * operands.last)
  }
}

// Dividing the numbers.
object DivideTheNumbers extends ValidateOperator {

  override def validate(operands: Seq[Double]): Boolean = {
    if (operands.size == 2)
      if (operands.last == 0) throw new ArithmeticException("Can't Divide By Zero")
      else true
    else false
  }

  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    Seq(operands.head / operands.last)
  }
}

// Power of the number.
object PowerOfTheNumber extends ValidateOperator {

  override def validate(operands: Seq[Double]): Boolean = {
    if (operands.size == 2) true
    else false
  }

  override protected def execute(operands: Seq[Double]): Seq[Double] = {

    @tailrec
    def calculatePower(operand: Double, power: Double): Double = {
      if (power == 0)
        operand
      else
        calculatePower(operand * operand, power - 1)
    }

    Seq(calculatePower(operands.head, operands.last))
  }

}

// Square root of the number.
object SquareRootOfTheNumber extends ValidateOperator {

  override def validate(operands: Seq[Double]): Boolean = {
    if (operands.size == 1)
      if (operands.head > 0) true else false
    else false
  }

  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    Seq(Math.sqrt(operands.head))
  }
}

// Factorial of a number.
object FactorialOfTheNumber extends ValidateOperator {

  override def validate(operands: Seq[Double]): Boolean = {
    if (operands.size == 1)
      if (operands.head > 0 && operands.head == operands.head.toInt) true else false
    else false
  }

  override protected def execute(operands: Seq[Double]): Seq[Double] = {

    @tailrec
    def calculateFactorial(number: Double, result: Double): Double = {

      if (number <= 1) result
      else
        calculateFactorial(number - 1, number * result)
    }

    Seq(calculateFactorial(operands.head, 1))
  }
}

// Sum of the numbers.
object SumTheNumbers extends ValidateOperator {

  override def validate(operands: Seq[Double]): Boolean = {
    if (operands.nonEmpty) true
    else false
  }

  override protected def execute(operands: Seq[Double]): Seq[Double] = {

    Seq(operands.reduce(_ + _))
  }
}

// GCD of the numbers.
object GcdOfNumbers extends ValidateOperator {

  override def validate(operands: Seq[Double]): Boolean = {
    if (operands.size == 2) true
    else false
  }

  override protected def execute(operands: Seq[Double]): Seq[Double] = {

    @tailrec
    def FindGcd(num1: Double, num2: Double): Double = {
      if (num2 == 0) num1 else FindGcd(num2, num1 % num2)
    }

    Seq(FindGcd(operands.head, operands.last))
  }
}


// Odd numbers in the Seq.
object OddNumbers extends ValidateOperator {

  override def validate(operands: Seq[Double]): Boolean = {
    if (operands.nonEmpty) true
    else false
  }

  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    operands.filter(_ % 2 == 0)
  }
}

// Even numbers in the Seq.
object EvenNumbers extends ValidateOperator {

  override def validate(operands: Seq[Double]): Boolean = {
    if (operands.nonEmpty) true
    else false
  }

  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    operands.filterNot(_ % 2 == 0)
  }
}

// Fibonacci .
object Fibonacci extends ValidateOperator {

  override def validate(operands: Seq[Double]): Boolean = {
    if (operands.size == 1) true
    else false
  }

  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    @tailrec
    def fibonacci(number: Double, first: Double, second: Double, list: Seq[Double]): Seq[Double] = {
      if (number == 0) list
      else fibonacci(number - 1, second, second + first, list :+ first)
    }

    fibonacci(operands.head, 0, 1, Seq())
  }
}

object ExpressionEvaluator {

  def squareOfExpression(firstOperand: Double, secondOperand: Double): String = {

    val add = AddTheNumbers
    val power = PowerOfTheNumber
    val mul = MultiplyTheNumbers
    val list = Seq(firstOperand, secondOperand)
    val LhsResult = power.validateAndExecute(add.validateAndExecute(list) ++ Seq(2.0))
    val RhsHalfResult = add.validateAndExecute(power.validateAndExecute(Seq(firstOperand, 2)) ++ power.validateAndExecute(Seq(secondOperand, 2)))
    val RhsTotalResult = add.validateAndExecute(RhsHalfResult ++ mul.validateAndExecute(mul.validateAndExecute(Seq(2, firstOperand)) ++ Seq(secondOperand)))
    if (LhsResult == RhsTotalResult) "Equal"
    else "Not Equal"
  }

  /* This method is used to find the number whose factorial is greater than 6^num */
  def find(numbers: Seq[Double]): Future[Seq[Double]] = {

    @tailrec
    def findFactorial(number: Double, result: Double): Double = {
      if (number <= 1) result
      else findFactorial(number - 1, result * number)
    }

    val finalResult = numbers.filter { num =>
      val result = findFactorial(num, 1)
      result > math.pow(6, num)
    }
    Future(finalResult)
  }

  /* This method is used to find the average after performing the fibonacci on each number, filter the odd elements */
  def findAverageAfterChainingOperations(numbers: Seq[Double]): Future[Double] = {
    Future {
      @tailrec
      def fibonacci(times: Double, numberOne: Double, numberTwo: Double): Double = {
        if (times <= 1) numberTwo
        else fibonacci(times - 1, numberTwo, numberOne + numberTwo)
      }

      val filteredDataNumbers = numbers.filter { num =>
        val result = fibonacci(num.toInt, 0, 1)
        result % 2 != 0
      }
      filteredDataNumbers.foldLeft(0.0)((numOne: Double, numTwo: Double) => numOne + numTwo) / filteredDataNumbers.size
    }
  }
}

object Calculator {

  def calculate(operator: String, operands: Seq[Double]): Future[Seq[Double]] = {

    operator match {
      case "+" => execute(AddTheNumbers, operands)
      case "-" => execute(SubtractTheNumbers, operands)
      case "*" => execute(MultiplyTheNumbers, operands)
      case "/" => execute(DivideTheNumbers, operands)
      case "^" => execute(PowerOfTheNumber, operands)
      case "sqrt" => execute(SquareRootOfTheNumber, operands)
      case "!" => execute(FactorialOfTheNumber, operands)
      case "sum" => execute(SumTheNumbers, operands)
      case "gcd" => execute(GcdOfNumbers, operands)
      case "odd" => execute(OddNumbers, operands)
      case "even" => execute(EvenNumbers, operands)
      case "fibonacci" => execute(Fibonacci, operands)
      case _ => throw new Exception
    }
  }

  def execute(operator: Operator, operands: Seq[Double]): Future[Seq[Double]] = {

    Future {
      operator.validateAndExecute(operands)
    }

  }
}
