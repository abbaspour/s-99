package io.github.abbaspour.s99

import scala.util.Random
import scala.annotation.tailrec

object P24 {
  def lotto(n : Int, m : Int) : List[Int] = {
    @tailrec
    def lottoR(n : Int, m : Int, res : List[Int]) : List[Int] = {
      if (n <= 0) res
      else lottoR(n - 1, m, Random.nextInt(m) :: res)
    }
    lottoR(n, m, Nil)
  }

}
