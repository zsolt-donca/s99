package com.zsd.s99.mtrees

import org.scalatest.{FlatSpec, Matchers}

class P71Test extends FlatSpec with Matchers {

  import P70._
  import P71._

  "The sample node string" should "have an internal path length of 9" in {
    val sampleNodeString = "afg^^c^bd^e^^^"

    val mTree: MTree[Char] = sampleNodeString.toMTree
    mTree.internalPathLength should be(9)
  }

}
