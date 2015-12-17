package com.zsd.s99.arithmetic

object P41 extends App {

  import P40._

  def goldbachList(range: Seq[Int]): Seq[(Int, Int, Int)] = {
    range.view.map(num => (num, goldbachConjecture(num))).collect { case (a, Some((b, c))) => (a, b, c) }
  }

  //noinspection ScalaUnnecessaryParentheses
  def goldbachListLimited(range: Seq[Int], limit: Int) = {
    goldbachList(range).filter { case (a, b, c) => (b >= limit && c >= limit) }
  }

  def goldbachListLimitedToString(range: Seq[Int], limit: Int) = {
    goldbachListLimited(range, limit).map { case (a, b, c) => s"$a = $b + $c" }
  }

  goldbachListLimitedToString(1 to 20000, 50) foreach println
}
