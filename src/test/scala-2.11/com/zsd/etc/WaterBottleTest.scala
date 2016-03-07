package com.zsd.etc

import org.scalatest.{FlatSpec, Matchers}

class WaterBottleTest extends FlatSpec with Matchers {

  import WaterBottle._
  import Op._
  import Side._

  "Pouring water" should "work as expected" in {
    applyOpFromLeft(State(0, 0), Capacity(3, 4), Pour) should be(State(0, 0))
    applyOpFromLeft(State(2, 0), Capacity(3, 4), Pour) should be(State(0, 2))
    applyOpFromLeft(State(2, 3), Capacity(3, 4), Pour) should be(State(1, 4))
    applyOpFromLeft(State(2, 4), Capacity(3, 4), Pour) should be(State(2, 4))
  }

  "Filling water" should "work as expected" in {
    applyOpFromLeft(State(0, 2), Capacity(3, 4), Fill) should be(State(3, 2))
    applyOpFromLeft(State(2, 2), Capacity(3, 4), Fill) should be(State(3, 2))
    applyOpFromLeft(State(3, 2), Capacity(3, 4), Fill) should be(State(3, 2))
  }

  "Emptying water" should "work as expected" in {
    applyOpFromLeft(State(0, 2), Capacity(3, 4), Empty) should be(State(0, 2))
    applyOpFromLeft(State(2, 2), Capacity(3, 4), Empty) should be(State(0, 2))
    applyOpFromLeft(State(3, 2), Capacity(3, 4), Empty) should be(State(0, 2))
  }

  "Applying action from the right" should "work as expected" in {
    applyAction(State(2, 3), Capacity(3, 4), Action(Pour, Right)) should be(State(3, 2))
    applyAction(State(2, 2), Capacity(3, 4), Action(Fill, Right)) should be(State(2, 4))
    applyAction(State(2, 2), Capacity(3, 4), Action(Empty, Right)) should be(State(2, 0))
  }

  "Best solutions for the sample problem" should "be as expected" in {
    val solutions = solve(Capacity(5, 3), desired = 4)
    val best = bestSolutions(solutions)
    best should be(Seq(Seq(State(0, 0), State(5, 0), State(2, 3), State(2, 0), State(0, 2), State(5, 2), State(4, 3))))
  }
}
