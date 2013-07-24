package io.github.abbaspour.s99

class S99Logic(a : Boolean) {

  import S99Logic._

  def or(a: Boolean, b: Boolean): Boolean = a match {
    case true => true
    case false => b
  }

  def and(a: Boolean, b: Boolean): Boolean = a match {
    case false => false
    case true => a
  }

  def equ(a: Boolean, b: Boolean): Boolean = or(and(a, b), and(not(a), not(b)))

  def xor(a: Boolean, b: Boolean): Boolean = not(equ(a, b))

  def nor(a: Boolean, b: Boolean): Boolean = not(or(a, b))

  def nand(a: Boolean, b: Boolean): Boolean = not(and(a, b))

  def impl(a: Boolean, b: Boolean): Boolean = or(not(a), b)

}

object S99Logic {
  implicit def boolean2S99Logic(a: Boolean): S99Logic = new S99Logic(a)

  def and(a: Boolean, b: Boolean): Boolean = a match {
    case false => false
    case true => a
  }

  def or(a: Boolean, b: Boolean): Boolean = a match {
    case true => true
    case false => b
  }

  def not(a: Boolean): Boolean = a match {
    case true => false
    case false => true
  }

  def table2(f: (Boolean, Boolean) => Boolean) {
    println("A     B     result")
    for {a <- List(true, false)
         b <- List(true, false)} {
      printf("%-5s %-5s %-5s\n", a, b, f(a, b))
    }
  }
}