package com.zsd

object P59 extends App {

  import P55._

  def hBalanced[T](height: Int, value: T): Seq[Tree[T]] = {
    generateBinaryTree(height, value) {
      height => {
        for (heightLeft <- Seq(height - 1);
             leftSize <- (heightLeft - 1) to heightLeft;
             rightSize <- (heightLeft - 1) to heightLeft
             if leftSize == heightLeft || rightSize == heightLeft)
          yield (leftSize, rightSize)
      }
    }
  }

  private val trees: Seq[Tree[String]] = hBalanced(3, "x")
  trees.foreach(tree => {
    println(s"height: ${tree.height}, size: ${tree.size}, tree: $tree")
  })
}
