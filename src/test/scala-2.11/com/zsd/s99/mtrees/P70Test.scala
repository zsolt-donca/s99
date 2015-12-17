package com.zsd.s99.mtrees

import org.scalatest.{FlatSpec, Matchers}

class P70Test extends FlatSpec with Matchers {

  import P70._

  "The sample node string" should "be converted into the expected multi-way tree" in {

    val sampleNodeString = "afg^^c^bd^e^^^"
    val expectedMTree = MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e')))))

    sampleNodeString.toMTree should be(expectedMTree)
  }
}
