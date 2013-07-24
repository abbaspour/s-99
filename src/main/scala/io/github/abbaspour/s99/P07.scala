package io.github.abbaspour.s99

object P07 {
  //todo @tailrec
  def flatten(as : List[Any]) : List[Any] = as flatMap {
    case l : List[_] => flatten(l)
    case e => List(e)
  }
}
