package com.zsd.s99.mtrees

import scala.collection.immutable.List

object P70 {

  implicit class NodeStringConversion(val nodeString: String) {
    def toMTree: MTree[Char] = {
      val stack = List[MTree[Char]](MTree(' '))
      val result = nodeString.toCharArray.foldLeft(stack)(processChar)

      result match {
        case List(MTree(' ', List(mTree))) => mTree
        case _ => throw new IllegalArgumentException("Invalid node string: " + nodeString)
      }
    }

    private def processChar(stack: List[MTree[Char]], ch: Char): List[MTree[Char]] = {
      (ch, stack) match {
        case ('^', first :: second :: rest) => MTree(second.value, second.children ++ List(first)) :: rest
        case ('^', _) => throw new IllegalArgumentException("Non-matching ^ character")
        case (node, _) => MTree(node) :: stack
      }
    }
  }

}
