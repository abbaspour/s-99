package io.github.abbaspour.s99

import scala.util.Random
import scala.annotation.tailrec

object P23 {
  def randomSelect[A](n : Int, ls : List[A]) : List[A] = {
    @tailrec
    def randomSelectR[A](n : Int, ls : List[A], res : List[A]) : List[A] = {
      if(n == 0) res
      else if (n > ls.size) ls
      else {
        val (l, e) = P20.removeAt(Random.nextInt(ls.size), ls)
        randomSelectR(n - 1, l, e :: res)
      }
    }
    randomSelectR(n, ls, Nil)
  }
}
