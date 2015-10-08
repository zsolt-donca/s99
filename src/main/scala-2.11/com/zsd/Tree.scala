package com.zsd

import java.lang.Math.max

sealed abstract class Tree[+T] {
  def size: Int

  def height: Int
}

case class TreeNode[+T](value: T, left: Tree[T], right: Tree[T], position: Option[Position]) extends Tree[T] {
  override def toString =
    position match {
      case Some(Position(x, y)) => "T[" + x.toString + "," + y.toString + "](" + value.toString + " " + left.toString + " " + right.toString + ")"
      case None => "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
    }

  override def size: Int = left.size + right.size + 1

  override def height: Int = max(left.height, right.height) + 1

  def hasPosition: Boolean = position.isDefined
}

case class Position(x: Int, y: Int)

case object End extends Tree[Nothing] {
  override def toString = "."

  override def size: Int = 0

  override def height: Int = 0
}

object Node {
  def apply[T](value: T): TreeNode[T] = TreeNode(value, End, End, None)

  def apply[T](value: T, left: Tree[T], right: Tree[T]): TreeNode[T] = TreeNode(value, left, right, None)

  def unapply[T](node: Tree[T]): Option[(T, Tree[T], Tree[T])] = {
    node match {
      case node: TreeNode[T] => Some(node.value, node.left, node.right)
      case _ => None
    }
  }
}

object NodeWithPosition {
  def apply[T](value: T, left: Tree[T], right: Tree[T], x: Int, y: Int): TreeNode[T] = TreeNode(value, left, right, Some(Position(x, y)))

  def unapply[T](node: Tree[T]): Option[(T, Tree[T], Tree[T], Int, Int)] = {
    node match {
      case node: TreeNode[T] =>
        node.position match {
          case Some(Position(x, y)) => Some((node.value, node.left, node.right, x, y))
          case _ => None
        }
      case _ => None
    }
  }
}

