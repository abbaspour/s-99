package io.github.abbaspour.s99

object P15 {
  def duplicateN[A](n : Int, xs : List[A]) : List[A] = xs.flatMap(List.fill(n)(_))
}
