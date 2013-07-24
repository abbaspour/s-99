package io.github.abbaspour.s99

object P10 {
  def encode[A](xs : List[A]) : List[(Int, A)] = P09.pack(xs).map { e: List[A] => (e.length, e.head)}
}
