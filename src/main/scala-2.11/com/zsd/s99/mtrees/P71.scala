package com.zsd.s99.mtrees

/**
  * Copyright (c) Neverfail Group 2015
  */
object P71 {

  implicit class InternalPathLength(val mTree: MTree[_]) {

    def internalPathLength: Int = {
      val internalPaths: List[List[Any]] = gatherInternalPaths(mTree)
      internalPaths.map(_.length).sum
    }

    private def gatherInternalPaths[T](mTree: MTree[T]): List[List[T]] = {
      mTree.children.map(t => List(t.value)) ++ mTree.children.flatMap(gatherInternalPaths).map(mTree.value :: _)
    }
  }

}
