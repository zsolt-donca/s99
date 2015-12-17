package com.zsd.s99.mtrees

case class MTree[+T](value: T, children: List[MTree[T]]) {
  def this(value: T) = this(value, List())

  override def toString = "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"
}

object MTree {
  def apply[T](value: T) = new MTree(value)

  //  def apply[T](value: T, children: List[MTree[T]]) = new MTree(value, children)
}
