package io.github.abbaspour.s99

import scala.annotation.tailrec

object P03 {
  @tailrec
  def nth[A](n : Int, list: List[A]) : A = n match {
    case 0 => list.head
    case x if x > 0 => nth(x - 1, list.tail)
    case _ => throw new NoSuchElementException
  }
}
