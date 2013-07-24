package io.github.abbaspour.s99

import io.github.abbaspour.s99.S99Int.int2S99Int

object P37 {
  def phi(m : Int) : Long = {
    val primeFactors = m.primeFactorMultiplicity()
    def f(z : Long, pf : (Int, Int)) : Long = z * (pf._1 - 1) * Math.pow(pf._1, pf._2 - 1).toLong
    primeFactors.foldLeft(1L : Long)(f)
  }
}
