package io.github.abbaspour.s99

object P27 {
  def group[A](sizing : List[Int], ls : Set[A]) : List[List[List[A]]] = {
    if (sizing.isEmpty) return List(Nil)
    if (ls.isEmpty) return List(List(Nil))
    for {
      e <- P26.combinations(sizing.head, ls.toList)
      rest = group(sizing.tail, ls -- e)
      r <- rest
    }
      yield e :: r
  }
}
