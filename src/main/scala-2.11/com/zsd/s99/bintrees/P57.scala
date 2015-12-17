package com.zsd.s99.bintrees

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
  assert(t1 == Node(2))

  val t2 = t1.addValue(3)
  assert(t2 == Node(2, End, Node(3)))

  val t3 = t2.addValue(0)
  assert(t3 == Node(2, Node(0), Node(3)))

  val t4 = fromList(List(3, 2, 5, 7, 1))
  assert(t4 == Node(3, Node(2, Node(1), End), Node(5, End, Node(7))))

  import P56._

  val r5 = fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric
  assert(r5)

  val r6 = fromList(List(3, 2, 5, 7, 4)).isSymmetric
  assert(!r6)
}
