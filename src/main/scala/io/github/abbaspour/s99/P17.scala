package io.github.abbaspour.s99

object P17 {
  def split[A](n : Int, ls : List[A]) : (List[A], List[A]) = (ls.take(n), ls.drop(n))
}
