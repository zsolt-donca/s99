package com.zsd.s99.bintrees

import scala.collection.immutable.Stream.from

object P66 extends TreeVisualizingApp {

  import P57._
  import TreeOperations._

  implicit class TreePositioning3[T](val tree: Tree[T]) {

    def layoutBinaryTree3: Tree[T] = {
      val layout = minTree(tree)
      val minX = layout.inOrder.collect({ case NodeWithPosition(_, _, _, x, _) => x }).min
      shift(layout, Position(-minX + 1, 1))
    }

    private def minTree(tree: Tree[T]): Tree[T] = {
      tree match {
        case Node(value, left, right) =>
          val firstNonClash = from(1).map(diff => separate(value, minTree(left), minTree(right), diff)).find(nonClash)
          firstNonClash.get
        case End => End
      }
    }

    private def separate(value: T, left: Tree[T], right: Tree[T], diff: Int): Tree[T] = {
      NodeWithPosition(value, shift(left, Position(-diff, 1)), shift(right, Position(diff, 1)), 0, 0)
    }

    private def shift(tree: Tree[T], diff: Position): Tree[T] = {
      tree match {
        case TreeNode(value, left, right, Some(position)) =>
          TreeNode[T](value, shift(left, diff), shift(right, diff), Some(position + diff))
        case End => End
      }
    }

    private def nonClash(tree: Tree[T]): Boolean = {
      tree match {
        case Node(_, left, right) => collectPositions(left).intersect(collectPositions(right)).isEmpty
        case End => true
      }
    }

    private def collectPositions(tree: Tree[T]): Set[Position] = {
      tree match {
        case TreeNode(_, left, right, Some(position)) => Set(position) ++ collectPositions(left) ++ collectPositions(right)
        case End => Set()
      }
    }
  }

  val title = "P65 - Layout a binary tree (3)"
  val tree = fromList(List('n', 'k', 'm', 'c', 'a', 'e', 'd', 'g', 'u', 'p', 'q'))
  val treeWithLayout = tree.layoutBinaryTree3
}
