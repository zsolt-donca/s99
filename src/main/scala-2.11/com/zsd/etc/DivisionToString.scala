package com.zsd.etc

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object DivisionToString {

  def divide(dividend: Int, divisor: Int): String = {

    if (divisor == 0) return "NaN"

    val isNegative = (dividend < 0) != (divisor < 0)
    val history = mutable.Map(0 -> -1)
    val results = ArrayBuffer[Any]()
    var distance = 1

    import Math._
    for (step <- getSteps(abs(dividend), abs(divisor))) {
      results += step.ratio

      def previousStep = history.get(step.next)
      if (previousStep.isEmpty) {
        history(step.next) = distance
        distance += 1
      }
      else {
        return toString(results, previousStep.get, isNegative)
      }
    }

    throw new IllegalStateException
  }

  def getSteps(dividend: Int, divisor: Int): Stream[DivisionStep] = {
    Stream.iterate(new DivisionStep(dividend, divisor)) {
      current => new DivisionStep(current.next, divisor)
    }
  }

  def toString(results: ArrayBuffer[Any], repetitionStart: Int, negative: Boolean) = {
    if (repetitionStart >= 0) {
      results.insert(repetitionStart, "(")
      results += ')'
    }

    if (results.length > 1) results.insert(1, ".")
    if (negative) results.insert(0, "-")
    results.mkString
  }

  case class DivisionStep(dividend: Int, divisor: Int) {
    val ratio = dividend / divisor
    val next = (dividend % divisor) * 10
  }
}
