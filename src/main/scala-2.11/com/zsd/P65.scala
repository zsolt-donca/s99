package com.zsd

object P65 extends TreeVisualizingApp {

  import P57._
  import TreeOperations._

  implicit class TreePositioning2[T](val tree: Tree[T]) {

    val treeDepths: Map[Tree[T], Int] = tree.depthMap

    val height = treeDepths.values.max

    private def layoutBinaryTree2(tree: Tree[T], parentPos: Int): Tree[T] = {
      tree match {
        case Node(value, left, right) =>
          val y = treeDepths(tree)
          val diff = 1 << (height - y - 1)
          Node(value, layoutBinaryTree2(left, parentPos - diff), layoutBinaryTree2(right, parentPos + diff), parentPos - 1, y + 1)
        case End => End
      }
    }

    def layoutBinaryTree2: Tree[T] = layoutBinaryTree2(tree, 1 << height)
  }

  val title = "P65 - Layout a binary tree (2)"
  val tree = fromList(List('n', 'k', 'm', 'c', 'a', 'e', 'd', 'g', 'u', 'p', 'q'))
  val treeWithLayout = tree.layoutBinaryTree2
}
