package io.github.abbaspour.s99

import scala.annotation.tailrec

object P32 {
  @tailrec
  def gcd(m : Int, n : Int) : Int = n match {
    case 0 => m
    case _ => gcd(n, m%n)
  }
}
