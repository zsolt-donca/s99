package com.zsd.s99.mtrees

import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.{FlatSpec, Matchers}

class P73Test extends FlatSpec with Matchers {

  import P70._
  import P73._

  "converting to lispy trees" should "be correct for the test examples" in {
    forAll(lispyTreeExamples) {
      (mTreeStr: String, expectedLispyTree: String) =>
        val mTree = mTreeStr.toMTree
        val lispyTree = mTree.lispyTree

        lispyTree should be(expectedLispyTree)
    }
  }

  val lispyTreeExamples = Table(
    ("mTree", "lispyTree"),
    ("a^", "a"),
    ("ab^^", "(a b)"),
    ("abc^^^", "(a (b c))"),
    ("bd^e^^", "(b d e)"),
    ("afg^^c^bd^e^^^", "(a (f g) c (b d e))")
  )
}
