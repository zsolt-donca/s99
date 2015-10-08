package com.zsd

import com.zsd.P57._

object P64 extends TreeVisualizingApp {

  implicit class TreePositioning1[T](val tree: Tree[T]) {

    import TreeOperations._

    val nodesInOrder: Map[Tree[T], Int] = (tree.inOrder zip Stream.from(1)).toMap
    val treeDepths: Map[Tree[T], Int] = tree.depthMap

    private def layoutBinaryTree(tree: Tree[T]): Tree[T] = {
      tree match {
        case Node(value, left, right) => Node(value, layoutBinaryTree(left), layoutBinaryTree(right), nodesInOrder(tree), treeDepths(tree) + 1)
        case End => End
      }
    }

    def layoutBinaryTree: Tree[T] = layoutBinaryTree(tree)
  }

  val title = "P64 - Layout a binary tree (1)"
  val tree = fromList(List('n', 'k', 'm', 'c', 'a', 'h', 'g', 'e', 'u', 'p', 's', 'q'))
  val treeWithLayout = tree.layoutBinaryTree
}
