package com.knoldus.calculator

trait Validator {
  //validate the operands for the specific operator
  def validate(operands: Seq[Double]): Boolean
}

trait Operator extends Validator {
  //validate and execute - implement this function in trait
  //this function will validate the operands and execute it.
  //throw CalculatorException when validation fails.
  def validateAndExecute(operands: Seq[Double]): Seq[Double]
  protected def execute(operands: Seq[Double]): Seq[Double]
}

trait ValidateOperator extends Operator {

  override def validateAndExecute(operands: Seq[Double]): Seq[Double] = {
    if (validate(operands)) execute(operands)
    else throw new Exception
  }
}
