package io.github.abbaspour.s99

import scala.annotation.tailrec

object P04 {
  def length(l : List[_]) : Int = length(0, l)

  @tailrec
  private def length(n : Int, l : List[_]) : Int = l match {
    case _ :: rest => length(n + 1, rest)
    case Nil => n
  }


}
