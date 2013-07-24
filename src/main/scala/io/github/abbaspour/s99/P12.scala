package io.github.abbaspour.s99

object P12 {
  def decode[A](list : List[(Int, A)]) : List[A] = list.flatMap{ e => List.fill(e._1)(e._2)}
}
