package io.github.abbaspour.s99

object P28 {
  def lsort[A](ls : List[List[A]]) : List[List[A]] = ls.sortBy(_.size)
}
