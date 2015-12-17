package com.zsd.s99.arithmetic

import java.lang.Math.sqrt

object P40 extends App {

  def isPrime(num: Int): Boolean = {
    (num > 1) && !(2 to sqrt(num).asInstanceOf[Int]).exists(i => num % i == 0)
  }

  def goldbachConjecture(num: Int): Option[(Int, Int)] = {
    if (num >= 4 && num % 2 == 0)
      (2 to (num - 2)).map(x => (x, num - x)).find { case (a, b) => isPrime(a) && isPrime(b) }
    else
      None
  }

  implicit class Goldbach(num: Int) {
    def goldbach = goldbachConjecture(num)
  }

  (4 to 100 by 2) foreach (x => {
    println(s"goldbach($x): ${x.goldbach}")
  })

  println(28.goldbach)
}
