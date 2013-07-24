package io.github.abbaspour.s99

import scala.annotation.tailrec

object P22 {
  def range(f : Int, t : Int) : List[Int] = {
    @tailrec
    def rangeR(f : Int, t : Int, ls : List[Int]) : List[Int] = {
      if (f > t) ls
      else rangeR(f + 1, t, f :: ls)
    }
    rangeR(f, t, Nil).reverse
  }
}
