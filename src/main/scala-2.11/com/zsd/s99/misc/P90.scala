package com.zsd.s99.misc

object P90 extends App {

  def queens(n: Int): Seq[Seq[Int]] = {

    def queens(list: Seq[Int]): Seq[Seq[Int]] = {
      if (list.size == n) {
        Seq(list)
      } else {
        for (i <- 1 to n
             if !list.contains(i);
             newList = list ++ Seq(i)
             if !clashDiagonals(newList);
             solution <- queens(newList))
          yield solution
      }
    }

    def clashDiagonals(list: Seq[Int]): Boolean = {
      def positions: Seq[(Int, Int)] = {
        for (row <- 1 to list.size;
             col = list(row - 1))
          yield (row, col)
      }

      def onMap(pos: (Int, Int)): Boolean = {
        pos match {
          case (row, col) => 1 <= row && row <= n && 1 <= col && col <= n
        }
      }

      def neighbors(pos: (Int, Int))(dist: Int): Seq[(Int, Int)] = {
        pos match {
          case (row, col) => Seq((row - dist, col - dist), (row - dist, col + dist), (row + dist, col - dist), (row + dist, col + dist))
        }
      }

      def diagonals(pos: (Int, Int)): Seq[(Int, Int)] = {
        (1 to n).flatMap(neighbors(pos)).filter(onMap)
      }

      def checked(pos: (Int, Int)) = {
        pos match {
          case (row, col) => list.size >= row && list(row - 1) == col
        }
      }

      positions.flatMap(diagonals).exists(checked)
    }

    queens(List())
  }

  val lists: Seq[Seq[Int]] = queens(8)
  lists.foreach(println)
}
