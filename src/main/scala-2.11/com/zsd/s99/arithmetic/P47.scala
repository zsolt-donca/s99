package com.zsd.s99.arithmetic

object P47 extends App {

  implicit class BooleanOperations(val a: Boolean) extends AnyVal {
    def and(b: Boolean) = a && b

    def or(b: Boolean) = a || b

    def nand(b: Boolean) = !(a && b)

    def nor(b: Boolean) = !(a || b)

    def xor(b: Boolean) = a ^ b

    def impl(b: Boolean) = !a || b

    def equ(b: Boolean) = a == b
  }

  def not(a: Boolean) = !a

  def table2(f: (Boolean, Boolean) => Boolean) = {
    println("A     B     result")
    for (a <- Seq(true, false); b <- Seq(true, false)) {
      println(f"$a%-5s $b%-5s ${f(a, b)}")
    }
  }

  table2((a: Boolean, b: Boolean) => a and (a or not(b)))
}
