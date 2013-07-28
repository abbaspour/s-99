package io.github.abbaspour.s99

object P07 {
  def flatten(as : List[Any]) : List[Any] = as flatMap {
    case l : List[_] => for (x <- l) yield x
    case e => e :: Nil
  }
}
