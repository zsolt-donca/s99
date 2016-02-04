package com.zsd.etc

import java.lang.Math._

import scala.annotation.tailrec

object DivisionToString {

  def divide(dividend: Int, divisor: Int): String = {
    if (divisor == 0) {
      "NaN"
    } else {
      val negative = (dividend < 0) != (divisor < 0)
      val resultSeq = (if (negative) Seq("-") else Seq.empty) ++ dividePositive(abs(dividend), abs(divisor))
      resultSeq.mkString
    }
  }

  def dividePositive(dividend: Int, divisor: Int): Seq[Any] = {
    @tailrec
    def divide(steps: Stream[DivisionStep], distance: Int, history: Map[Int, Int], previousResult: List[Int]): Seq[Any] = {
      val (step #:: restOfSteps) = steps
      val result = step.ratio :: previousResult
      history.get(step.next) match {
        case Some(repetitionStart) => insertPunctuation(result.reverse.toSeq, repetitionStart)
        case None => divide(restOfSteps, distance + 1, history.updated(step.next, distance), result)
      }
    }

    val steps = Stream.iterate(new DivisionStep(dividend, divisor))(current => new DivisionStep(current.next, divisor))
    divide(steps, 1, Map(0 -> -1), List.empty)
  }

  def insertPunctuation(results: Seq[Int], repStart: Int): Seq[Any] = {
    if (results.length == 1) {
      results.toSeq
    } else if (repStart >= 0) {
      Seq(results.head, ".") ++ results.slice(1, repStart) ++ Seq("(") ++ results.slice(repStart, results.length) ++ Seq(")")
    } else {
      Seq(results.head, ".") ++ results.slice(1, results.length)
    }
  }

  class DivisionStep(dividend: Int, divisor: Int) {
    val ratio = dividend / divisor
    val next = (dividend % divisor) * 10
  }
}
