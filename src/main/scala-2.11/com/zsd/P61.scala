package com.zsd

object P61 extends App {

  implicit class CountingLeavesOfTree[T](val tree: Tree[T]) {
    def leafCount: Int = {
      tree match {
        case Node(_, End, End) => 1
        case Node(_, left, right) => left.leafCount + right.leafCount
        case End => 0
      }
    }
  }

  println(Node('x', Node('x'), End).leafCount)
}
