package com.zsd

object P58 extends App {

  import P55._
  import P56._

  def symmetricBalancedTrees[T](nodes: Int, value: T): Seq[Tree[T]] = {
    cBalanced(nodes, value).filter(_.isSymmetric)
  }

  val t1 = symmetricBalancedTrees(5, "x")

  println(t1)
  assert(t1.length == 2)
  assert(!t1.exists(tree => tree.size != 5 || !tree.isSymmetric))
}
