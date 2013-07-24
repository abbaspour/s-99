package io.github.abbaspour.s99

import scala.annotation.tailrec

object P02 {

  @tailrec
  def penultimate[A](list:List[A]) : A = list match {
    case a :: b :: Nil => a
    case a :: rest => penultimate(rest)
    case _ => throw new NoSuchElementException
  }

}
