package com.zsd

object P61A extends App {

  implicit class CollectingLeavesOfTree[T](val tree: Tree[T]) {
    def leafList: Seq[T] = {
      tree match {
        case Node(value, End, End) => Seq(value)
        case Node(_, left, right) => left.leafList ++ right.leafList
        case End => Seq()
      }
    }
  }

  println(Node('a', Node('b'), Node('c', Node('d'), Node('e'))).leafList)
}
