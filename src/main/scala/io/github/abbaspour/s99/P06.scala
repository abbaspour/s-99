package io.github.abbaspour.s99

import scala.annotation.tailrec

object P06 {
  @tailrec
  def isPalindrome[A](l : List[A]) : Boolean = l match {
    case h :: Nil => true
    case Nil => false
    case h :: tail => h == l.last && isPalindrome(l.tail.dropRight(1))
  }
}
