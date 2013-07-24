package io.github.abbaspour.s99

object P20 {
  def removeAt[A](n : Int, ls : List[A]) : (List[A], A) = {
    val (a, b) = ls.splitAt(n)
    (a ::: b.tail, b.head)
  }
}
