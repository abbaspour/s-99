package io.github.abbaspour.s99

object P19 {
  def rotate[A](n : Int, ls : List[A]) : List[A] = {
    val n1 = n % ls.size
    val n2 = if (n1 >= 0) n else ls.size + n1
    val (a, b) = ls.splitAt(n2)
    b ::: a
  }
}
