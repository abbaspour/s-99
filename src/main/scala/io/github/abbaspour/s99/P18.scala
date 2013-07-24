package io.github.abbaspour.s99

object P18 {
   def slice[A](f : Int, t: Int, ls : List[A]) : List[A] = ls.drop(f).take(t - f)
}
