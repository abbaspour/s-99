package io.github.abbaspour.s99

object P11 {
  def encodeModified[A](xs : List[A]) : List[Any] = P10.encode(xs).map {
    e: (Int, A) => if(e._1 == 1) e._2 else e
  }
}
