package io.github.abbaspour.s99

object P08 {

  def compress[A](xs : List[A]) : List[A] = xs match {
    case Nil => Nil
    case e :: Nil => e :: Nil
    case h :: tail =>
    tail.foldLeft(List(h)) { (b, a) => if (b.head == a) b else a :: b}.reverse
  }
}
