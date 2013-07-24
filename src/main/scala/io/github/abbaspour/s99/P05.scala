package io.github.abbaspour.s99

import scala.annotation.tailrec

object P05 {

  def reverse[A](l : List[A]) : List[A] = {
    @tailrec
    def reverseR[A](l : List[A], r : List[A]) : List[A] = l match {
      case h :: tail => reverseR(tail, h :: r )
      case Nil => r
      case _ => throw new NoSuchElementException
    }
    reverseR(l, Nil)
  }

  def reverseFold[A](l : List[A]) : List[A] = l.foldLeft(Nil : List[A]){ (r: List[A], e : A) => e :: r}
}
