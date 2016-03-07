package com.zsd.etc

import java.lang.Math._

object WaterBottle extends App {

  object Op extends Enumeration {
    val Pour, Fill, Empty = Value
  }

  object Side extends Enumeration {
    val Left, Right = Value
  }

  case class Action(op: Op.Value, side: Side.Value)

  case class State(left: Int, right: Int) {
    def flip = State(right, left)
  }

  case class Capacity(left: Int, right: Int) {
    def flip = Capacity(right, left)
  }

  def applyAction(state: State, capacity: Capacity, action: Action): State = {
    import Side._
    action.side match {
      case Left =>
        applyOpFromLeft(state, capacity, action.op)
      case Right =>
        applyOpFromLeft(state.flip, capacity.flip, action.op).flip
    }
  }

  def applyOpFromLeft(state: State, capacity: Capacity, op: Op.Value): State = {
    import Op._
    op match {
      case Pour =>
        val sum = state.left + state.right
        State(left = max(0, sum - capacity.right), right = min(sum, capacity.right))
      case Fill =>
        State(capacity.left, state.right)
      case Empty =>
        State(0, state.right)
    }
  }

  def solve(capacity: Capacity, desired: Int): Seq[Seq[State]] = {
    solve(State(0, 0), capacity, desired, List.empty)
  }

  def solve(state: State, capacity: Capacity, desired: Int, seen: List[State]): Seq[Seq[State]] = {
    if (state.left == desired || state.right == desired) {
      Seq((state +: seen).reverse)
    } else {
      for (op: Op.Value <- Seq(Op.Pour, Op.Fill, Op.Empty);
           side: Side.Value <- Seq(Side.Left, Side.Right);
           action = Action(op, side);
           newState = applyAction(state, capacity, action)
           if state != newState && !seen.contains(newState);
           results <- solve(newState, capacity, desired, state +: seen))
        yield results
    }
  }

  def bestSolutions(solutions: Seq[Seq[State]]): Seq[Seq[State]] = {
    val bestSize = solutions.map(_.size).min
    solutions.filter(_.size == bestSize)
  }
}
