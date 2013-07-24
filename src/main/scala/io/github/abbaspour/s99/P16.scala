package io.github.abbaspour.s99

object P16 {
  def drop[A](n : Int, xs : List[A]) : List[A] = xs.zipWithIndex.filter(e => e._2 % n != 0 ).map(_._1)
}
