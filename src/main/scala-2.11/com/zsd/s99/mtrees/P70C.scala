package com.zsd.s99.mtrees

object P70C extends App {

  implicit class NodeCount(val mTree: MTree[_]) {
    def nodeCount: Int = 1 + mTree.children.map(_.nodeCount).sum
  }

  println(MTree('a', List(MTree('f'))).nodeCount)
}
