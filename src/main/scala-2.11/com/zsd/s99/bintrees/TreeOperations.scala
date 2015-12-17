package com.zsd.s99.bintrees

object TreeOperations {

  implicit class Depth[T](val tree: Tree[T]) {
    private def treeDepth(tree: Tree[T], depth: Int): Seq[(Tree[T], Int)] = {
      tree match {
        case Node(_, left, right) => treeDepth(left, depth + 1) ++ Seq((tree, depth)) ++ treeDepth(right, depth + 1)
        case End => Seq()
      }
    }

    def depthMap: Map[Tree[T], Int] = treeDepth(tree, 0).toMap
  }

  implicit class InOrder[T](val tree: Tree[T]) {
    private def inOrder(tree: Tree[T]): Seq[Tree[T]] = {
      tree match {
        case Node(_, left, right) => inOrder(left) ++ Seq(tree) ++ inOrder(right)
        case End => Seq()
      }
    }

    def inOrder: Seq[Tree[T]] = inOrder(tree)
  }

}
