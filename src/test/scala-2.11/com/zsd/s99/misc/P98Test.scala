package com.zsd.s99.misc

import org.scalatest.{FlatSpec, Matchers}

class P98Test extends FlatSpec with Matchers {

  import P98._

  "The constraints of groups in lines" should "return as expected" in {
    constraints(Groups(5), 5 unknown) should be(Seq(0 to 4))

    divideGroups(Groups(2, 3)) should be(Seq((Seq(), Group(2), Seq(Group(3))), (Seq(Group(2)), Group(3), Seq())))
    divideGroups(Groups(2, 3, 4)) should be(Seq((Seq(), Group(2), Seq(Group(3), Group(4))), (Seq(Group(2)), Group(3), Seq(Group(4))), (Seq(Group(2), Group(3)), Group(4), Seq())))

    constraints(Groups(2, 3), 8 unknown) should be(Seq(0 to 3, 3 to 7))
    constraints(Groups(3, 3), 8 unknown) should be(Seq(0 to 3, 4 to 7))

    fillKnownPositions(Seq(2, 3), Seq(0 to 3, 3 to 7), 8 unknown) should be((5 unknown) ++ (1 filled) ++ (2 unknown))
    fillKnownPositions(Seq(3, 3), Seq(0 to 3, 4 to 7), 8 unknown) should be("?oo??oo?" asLine)
    fillKnownPositions(Seq(5), Seq(0 to 4), 5 unknown) should be(5 filled)

  }
}
