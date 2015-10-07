package com.zsd

import java.lang.Math.max

sealed abstract class Tree[+T] {
  def size: Int

  def height: Int
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

  override def size: Int = left.size + right.size + 1

  override def height: Int = max(left.height, right.height) + 1
}

case object End extends Tree[Nothing] {
  override def toString = "."

  override def size: Int = 0

  override def height: Int = 0
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}
