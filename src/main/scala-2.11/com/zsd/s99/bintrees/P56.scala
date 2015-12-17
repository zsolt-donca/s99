package com.zsd.s99.bintrees

object P56 extends App {

  def flipTree[T](tree: Tree[T]): Tree[T] = {
    tree match {
      case Node(value, left, right) => Node(value, flipTree(right), flipTree(left))
      case End => End
    }
  }

  def isSameStructure[T](a: Tree[T], b: Tree[T]): Boolean = {
    (a, b) match {
      case (Node(_, left1, right1), Node(_, left2, right2)) => isSameStructure(left1, left2) && isSameStructure(right1, right2)
      case (End, End) => true
      case _ => false
    }
  }

  implicit class TreeSymmetry[T](val tree: Tree[T]) {
    def isSymmetric = tree match {
      case Node(_, left, right) => isSameStructure(left, flipTree(right))
      case End => true
    }
  }

  println(Node('a', Node('b'), Node('c')).isSymmetric)
  println(Node('a', Node('b', End, Node('x', End, End)), Node('c')).isSymmetric)
  println(Node('a', Node('b', End, Node('x', End, End)), Node('c', Node('x', End, End), End)).isSymmetric)
}
