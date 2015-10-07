package com.zsd

import java.lang.Math.max

sealed abstract class Tree[+T] {
  def size: Int

  def height: Int
}

case class TreeNode[+T](value: T, left: Tree[T], right: Tree[T], x: Int, y: Int) extends Tree[T] {
  override def toString = {
    if (x < 0 && y < 0)
      "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
    else
      "T[" + x.toString + "," + y.toString + "](" + value.toString + " " + left.toString + " " + right.toString + ")"
  }

  override def size: Int = left.size + right.size + 1

  override def height: Int = max(left.height, right.height) + 1
}

case object End extends Tree[Nothing] {
  override def toString = "."

  override def size: Int = 0

  override def height: Int = 0
}

object Node {
  def apply[T](value: T): TreeNode[T] = Node(value, End, End)

  def apply[T](value: T, left: Tree[T], right: Tree[T]): TreeNode[T] = Node(value, left, right, -1, -1)

  def apply[T](value: T, left: Tree[T], right: Tree[T], x: Int, y: Int): TreeNode[T] = TreeNode(value, left, right, x, y)

  def unapply[T](node: Tree[T]): Option[(T, Tree[T], Tree[T])] = {
    node match {
      case node: TreeNode[T] => Some(node.value, node.left, node.right)
      case _ => None
    }
  }
}

