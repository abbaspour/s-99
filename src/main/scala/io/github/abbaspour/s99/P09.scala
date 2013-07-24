package io.github.abbaspour.s99

import scala.collection.immutable.List

object P09 {

  def pack[A](xs : List[A]) : List[List[A]] = xs match {
    case Nil => Nil
    case e :: Nil => List(List(e))
    case h :: tail =>
      tail.foldLeft(List(List(h))) {
        (b, a) => if(b.head.head == a) (a :: b.head) :: b.tail else List(a) :: b
      }.reverse
  }

}
