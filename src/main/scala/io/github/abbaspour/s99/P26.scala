package io.github.abbaspour.s99

object P26 {

  def combinations[A](n : Int, xs : List[A]) : List[List[A]] = {
    if(n == 0 || xs.isEmpty) return List(Nil)
    for {
      e <- xs.zipWithIndex
      rs = xs.drop(e._2 + 1)
      c <- combinations(n - 1, rs) if rs.length >= n - 1
    }
       yield e._1 :: c
  }

  def perms[A](n : Int, xs : List[A]) : List[List[A]] = {
    if(n == 0 || xs.isEmpty) return List(Nil)
    for {
      e <- xs.zipWithIndex
      rs = P20.removeAt(e._2, xs)._1
      c <- perms(n - 1, rs) if rs.length >= n - 1
    }
      yield e._1 :: c
  }

}
