package io.github.abbaspour.s99

object P13 {

  // todo: @tailrec
  def encodeDirect[A](xs : List[A]) : List[(Int, A)] = xs match {
    case Nil => Nil
    case _ => val (a, b) = xs.span(_ == xs.head)
    (a.length, a.head) :: encodeDirect(b)
  }
}
