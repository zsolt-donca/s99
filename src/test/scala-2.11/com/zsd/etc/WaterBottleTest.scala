package com.zsd.etc

import org.scalatest.{FlatSpec, Matchers}

class WaterBottleTest extends FlatSpec with Matchers {

  import WaterBottle._
  import Op._
  import Side._

  "Pouring water" should "work as expected" in {
    applyOpFromLeft(Pour, State(0, 0), Capacity(3, 4)) should be(State(0, 0))
    applyOpFromLeft(Pour, State(2, 0), Capacity(3, 4)) should be(State(0, 2))
    applyOpFromLeft(Pour, State(2, 3), Capacity(3, 4)) should be(State(1, 4))
    applyOpFromLeft(Pour, State(2, 4), Capacity(3, 4)) should be(State(2, 4))
  }

  "Filling water" should "work as expected" in {
    applyOpFromLeft(Fill, State(0, 2), Capacity(3, 4)) should be(State(3, 2))
    applyOpFromLeft(Fill, State(2, 2), Capacity(3, 4)) should be(State(3, 2))
    applyOpFromLeft(Fill, State(3, 2), Capacity(3, 4)) should be(State(3, 2))
  }

  "Emptying water" should "work as expected" in {
    applyOpFromLeft(Empty, State(0, 2), Capacity(3, 4)) should be(State(0, 2))
    applyOpFromLeft(Empty, State(2, 2), Capacity(3, 4)) should be(State(0, 2))
    applyOpFromLeft(Empty, State(3, 2), Capacity(3, 4)) should be(State(0, 2))
  }

  "Applying action from the right" should "work as expected" in {
    applyAction(Action(Pour, Right), State(2, 3), Capacity(3, 4)) should be(State(3, 2))
    applyAction(Action(Fill, Right), State(2, 2), Capacity(3, 4)) should be(State(2, 4))
    applyAction(Action(Empty, Right), State(2, 2), Capacity(3, 4)) should be(State(2, 0))
  }

  "Best solutions for the sample problem" should "be as expected" in {
    bestSolution(solve(Capacity(5, 3), desired = 4)) should be(Seq(State(0, 0), State(5, 0), State(2, 3), State(2, 0), State(0, 2), State(5, 2), State(4, 3)))
  }
}
