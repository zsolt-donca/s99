package com.zsd

import java.lang.Math._

object P60 extends App {

  def minHBalNodes(height: Int): Int = {
    if (height <= 0)
      0
    else if (height == 1)
      1
    else {
      minHBalNodes(height - 2) + minHBalNodes(height - 1) + 1
    }
  }

  def maxHBalHeight(nodes: Int): Int = {
    Stream.from(0).find(minHBalNodes(_) >= nodes).get
  }

  def hBalTreesWithNodes[T](nodes: Int, value: T): Seq[Tree[T]] = {
    if (nodes <= 0)
      Seq(End)
    else if (nodes == 1)
      Seq(Node(value))
    else {
      val nodesLeft = nodes - 1
      for (leftSize <- (nodesLeft / 2 - 1) to nodesLeft / 2 + 1;
           rightSize = nodesLeft - leftSize
           if abs(leftSize - rightSize) <= 1;
           leftTree <- hBalTreesWithNodes(leftSize, value);
           rightTree <- hBalTreesWithNodes(rightSize, value)
           if abs(leftTree.height - rightTree.height) <= 1
      ) yield Node(value, leftTree, rightTree)
    }
  }


  println(s"minHbalNodes(3) = ${minHBalNodes(3)}")

  println(s"maxHBalHeight(4) = ${maxHBalHeight(4)}")

  val trees = hBalTreesWithNodes(4, "x")
  trees.foreach(tree => {
    println(s"height: ${tree.height}, size: ${tree.size}, tree: $tree")
  })

  println(s"Number of height-balanced trees with the size of 15: ${hBalTreesWithNodes(15, "x").length}")
}
