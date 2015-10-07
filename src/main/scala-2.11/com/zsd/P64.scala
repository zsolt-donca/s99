package com.zsd

import com.zsd.P57._

object P64 extends TreeVisualizingApp {

  implicit class TreePositioning1[T](val tree: Tree[T]) {

    private def inOrder(tree: Tree[T]): Seq[Tree[T]] = {
      tree match {
        case Node(_, left, right) => inOrder(left) ++ Seq(tree) ++ inOrder(right)
        case End => Seq()
      }
    }

    private def treeDepth(tree: Tree[T], depth: Int): Seq[(Tree[T], Int)] = {
      tree match {
        case Node(_, left, right) => treeDepth(left, depth + 1) ++ Seq((tree, depth)) ++ treeDepth(right, depth + 1)
        case End => Seq()
      }
    }

    def inOrder: Seq[Tree[T]] = inOrder(tree)

    val nodesInOrder: Map[Tree[T], Int] = (inOrder(tree) zip Stream.from(1)).toMap
    val treeDepths: Map[Tree[T], Int] = treeDepth(tree, 1).toMap

    private def layoutBinaryTree(tree: Tree[T]): Tree[T] = {
      tree match {
        case Node(value, left, right) => Node(value, layoutBinaryTree(left), layoutBinaryTree(right), nodesInOrder(tree), treeDepths(tree))
        case End => End
      }
    }

    def layoutBinaryTree: Tree[T] = layoutBinaryTree(tree)
  }

  val title = "P64 - Layout a binary tree (1)"
  val tree = fromList(List('n', 'k', 'm', 'c', 'a', 'h', 'g', 'e', 'u', 'p', 's', 'q'))
  val treeWithLayout = tree.layoutBinaryTree
}
