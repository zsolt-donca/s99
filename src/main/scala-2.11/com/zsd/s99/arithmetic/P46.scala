package com.zsd.s99.arithmetic

object P46 extends App {

  def and(a: Boolean, b: Boolean) = a && b

  def or(a: Boolean, b: Boolean) = a || b

  def nand(a: Boolean, b: Boolean) = !(a && b)

  def nor(a: Boolean, b: Boolean) = !(a || b)

  def xor(a: Boolean, b: Boolean) = a ^ b

  def impl(a: Boolean, b: Boolean) = !a || b

  def equ(a: Boolean, b: Boolean) = a == b

  def not(a: Boolean) = !a

  def table2(f: (Boolean, Boolean) => Boolean) = {
    println("A     B     result")
    for (a <- Seq(true, false); b <- Seq(true, false)) {
      println(f"$a%-5s $b%-5s ${f(a, b)}")
    }
  }

  table2((a: Boolean, b: Boolean) => and(a, or(a, b)))
}
