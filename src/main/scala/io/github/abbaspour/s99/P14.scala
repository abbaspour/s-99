package io.github.abbaspour.s99

object P14 {
  def duplicate[A](xs : List[A]) : List[A] = xs flatMap { e => List(e, e) }
}
