package com.zsd.etc

import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.Tables.Table
import org.scalatest.{Matchers, FlatSpec}

class DivisionToStringTest extends FlatSpec with Matchers {

  import DivisionToString._

  "dividing to string" should "work as expected" in {
    forAll(divisionExamples) {
      (divident: Int, divisor: Int, result: String) =>
        divide(divident, divisor) should be(result)
    }
  }

  val divisionExamples = Table(
    ("dividend", "divisor", "result"),
    (1         , 0        , "NaN"),        // illegal division
    (10        , 2        , "5"),          // no fraction
    (1         , 2        , "0.5"),        // no repetition
    (1         , 3        , "0.(3)"),      // simple repetition
    (22        , 7        , "3.(142857)"), // longer repeating part
    (11        , 6        , "1.8(3)"),     // non-repeating fractional part

    (-1        ,  4       , "-0.25"),      // negative divident
    ( 1        , -3       , "-0.(3)"),     // negative divisor
    (-1        , -7       , "0.(142857)"), // negative both

    (5         , 74       , "0.0(675)"),
    (1         , 28       , "0.03(571428)"),
    (1         , 24       , "0.041(6)"),
    (1         , 29       , "0.(0344827586206896551724137931)")
  )
}
