package com.zsd.etc

import java.lang.Math._

object WaterBottle extends App {
  object Op extends Enumeration { val Pour, Fill, Empty = Value } 
  object Side extends Enumeration { val Left, Right = Value }

  case class Action(op: Op.Value, side: Side.Value)
  case class State(left: Int, right: Int) { def swap = State(right, left) } 
  case class Capacity(left: Int, right: Int) { def swap = Capacity(right, left) }

  def applyAction(action: Action, state: State, capacity: Capacity) = {
    import Side._
    action.side match {
      case Left  ⇒ applyOpFromLeft(action.op, state, capacity)
      case Right ⇒ applyOpFromLeft(action.op, state.swap, capacity.swap).swap
    }
  }

  def applyOpFromLeft(op: Op.Value, state: State, capacity: Capacity) = {
    import Op._
    val sum = state.left + state.right
    op match {
      case Pour  ⇒ State(max(0, sum - capacity.right), min(sum, capacity.right))
      case Fill  ⇒ State(capacity.left, state.right)
      case Empty ⇒ State(0, state.right)
    }
  }

  def solve(capacity: Capacity, desired: Int, state: State = State(0, 0), seen: Seq[State] = Seq.empty): Seq[Seq[State]] = {
    val solution = state +: seen
    if (Seq(state.left, state.right) contains desired)
      Seq(solution.reverse)
    else
      for (op   ← Op.values.toSeq;
           side ← Side.values.toSeq;

           newState = applyAction(Action(op, side), state, capacity)
           if state != newState && !seen.contains(newState);

           results ← solve(capacity, desired, newState, solution)
      ) yield results
  }

  def bestSolution(solutions: Seq[Seq[State]]) = solutions.min(Ordering.by((_: Seq[State]).size))
}
