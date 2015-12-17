package com.zsd.s99.bintrees

import java.lang.Math.abs

object P55 extends App {
  def cBalanced[T](nodes: Int, value: T): Seq[Tree[T]] = {
    generateBinaryTree(nodes, value) {
      nodes => {
        val nodesLeft = nodes - 1
        for (leftSize <- (nodesLeft / 2 - 1) to nodesLeft / 2 + 1;
             rightSize = nodesLeft - leftSize
             if abs(leftSize - rightSize) <= 1
        ) yield (leftSize, rightSize)
      }
    }
  }

  def generateBinaryTree[T](num: Int, value: T)(f: (Int => Seq[(Int, Int)])): Seq[Tree[T]] = {
    if (num <= 0)
      Seq(End)
    else if (num == 1)
      Seq(Node(value))
    else {
      for ((leftNum, rightNum) <- f(num);
           leftTree <- generateBinaryTree(leftNum, value)(f);
           rightTree <- generateBinaryTree(rightNum, value)(f)
      ) yield Node(value, leftTree, rightTree)
    }
  }

  private val trees: Seq[Tree[String]] = cBalanced(4, "x")
  trees.foreach(tree => {
    println(s"size: ${tree.size}, tree: $tree")
  })
}
