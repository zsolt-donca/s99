package com.zsd

object P41 extends App {

  import P40._

  def goldbachList(range: Seq[Int]): Seq[(Int, Int, Int)] = {
    range.view.filter(num => num >= 4 && num % 2 == 0).map(num => (num, goldbachConjecture(num))).map { case (a, (b, c)) => (a, b, c) }
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
