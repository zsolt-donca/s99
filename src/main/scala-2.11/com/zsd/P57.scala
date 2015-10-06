package com.zsd

object P57 extends App {

  implicit class BinaryTreeOperations[T](val tree: Tree[T]) {
    def addValue[U >: T](value: U)(implicit ev1: U => Ordered[U]): Tree[U] = {
      tree match {
        case Node(x, left, right) =>
          if (x < value) {
            Node(x, left, right.addValue(value))
          } else if (x == value) {
            tree
          } else if (x > value) {
            Node(x, left.addValue(value), right)
          } else {
            throw new IllegalStateException()
          }
        case End => Node(value)
      }
    }
  }

  def fromList[U](list: Seq[U])(implicit ev1: U => Ordered[U]): Tree[U] = {
    list.foldLeft(End.asInstanceOf[Tree[U]])((a, b) => a.addValue(b))
  }

  val t1 = End.addValue(2)
  println(t1)

  val t2 = t1.addValue(3)
  println(t2)

  val t3 = t2.addValue(0)
  println(t3)

  val t4 = fromList(List(3, 2, 5, 7, 1))
  println(t4)

  import P56._

  val r5 = fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric
  println(r5)

  val r6 = fromList(List(3, 2, 5, 7, 4)).isSymmetric
  println(r6)
}
