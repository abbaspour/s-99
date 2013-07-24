package io.github.abbaspour.s99

object P21 {
  def insertAt[A](e : A, n : Int, ls : List[A]) : List[A] = {
    val (a, b) = ls.splitAt(n)
    a ::: (e :: b)
  }
}
