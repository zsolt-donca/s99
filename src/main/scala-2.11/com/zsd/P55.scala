package com.zsd

import java.lang.Math.abs

object P55 extends App {

  def cBalanced[T](nodes: Int, value: T): Seq[Tree[T]] = {
    if (nodes <= 0)
      Seq(End)
    else if (nodes == 1)
      Seq(Node(value))
    else {
      val nodesLeft = nodes - 1
      for (leftSize <- (nodesLeft / 2 - 1) to nodesLeft / 2 + 1;
           rightSize = nodesLeft - leftSize
           if abs(leftSize - rightSize) <= 1;
           leftTree <- cBalanced(leftSize, value);
           rightTree <- cBalanced(rightSize, value)
      ) yield Node(value, leftTree, rightTree)
    }
  }

  private val trees: Seq[Tree[String]] = cBalanced(4, "x")
  trees.foreach(tree => {
    println(s"size: ${tree.size}, tree: $tree")
  })
}
