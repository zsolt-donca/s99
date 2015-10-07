package com.zsd

object P59 extends App {

  def hBalanced[T](height: Int, value: T): Seq[Tree[T]] = {
    if (height <= 0)
      Seq(End)
    else if (height == 1)
      Seq(Node(value))
    else {
      val heightLeft = height - 1
      for (leftSize <- (heightLeft - 1) to heightLeft;
           rightSize <- (heightLeft - 1) to heightLeft
           if leftSize == heightLeft || rightSize == heightLeft;
           leftTree <- hBalanced(leftSize, value);
           rightTree <- hBalanced(rightSize, value)
      ) yield Node(value, leftTree, rightTree)
    }
  }

  private val trees: Seq[Tree[String]] = hBalanced(3, "x")
  trees.foreach(tree => {
    println(s"height: ${tree.height}, size: ${tree.size}, tree: $tree")
  })
}
