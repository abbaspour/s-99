package io.github.abbaspour.s99

import scala.annotation.tailrec

object P01 {

  @tailrec
  def lastInList[A](l : List[A]) : A = l match {
    case e :: Nil => e
    case _ :: rest => lastInList(rest)
    case _ => throw new NoSuchElementException
  }

}
