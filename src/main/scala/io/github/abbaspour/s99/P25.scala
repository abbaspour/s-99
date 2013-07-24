package io.github.abbaspour.s99

object P25 {
  def randomPermute[A](ls : List[A]) : List[A] = P23.randomSelect(ls.size, ls)
}
