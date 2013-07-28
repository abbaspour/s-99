package io.github.abbaspour.s99

import scala.language.implicitConversions
import scala.NoSuchElementException
import scala.collection.immutable.Range
import scala.annotation.tailrec

class S99Int(val start: Int) {

  def isCoprimeTo(n : Int) : Boolean = P32.gcd(start, n) == 1

  def totient : Int = {
    var sum = 0
    for {
      n <- 1 until start
      if P32.gcd(start, n) == 1
    }
      sum += 1
    sum
  }

  def primeFactors() : List[Int] = primeFactors(start)

  def primeFactors(n : Int) : List[Int] = {

    @tailrec
    def primeFactorsR2(n : Int, p: Int, pItr: Iterator[Int], result : List[Int]) : List[Int] =
      if(n == 1 || p > n) result
      else if(n % p == 0) primeFactorsR2(n / p, p, pItr, p :: result)
      else primeFactorsR2(n, pItr.next(), pItr, result)

    val pItr = S99Int.primesStream.iterator
    primeFactorsR2(n, pItr.next(), pItr, Nil).reverse
  }

  def primeFactorMultiplicity() : Map[Int, Int] = primeFactorMultiplicity(start)

  def primeFactorMultiplicity(n : Int) : Map[Int, Int] = {

    @tailrec
    def primeFactorMultiplicityR(n : Int, p: Int, pItr: Iterator[Int], result : Map[Int, Int]) : Map[Int, Int] = {
      if(n == 1 || p > n) return result
      if(n % p == 0)
        primeFactorMultiplicityR(n / p, p, pItr, result.updated(p, result.getOrElse(p, 0) + 1))
      else
        primeFactorMultiplicityR(n, pItr.next(), pItr, result)
    }

    val pItr = S99Int.primesStream.iterator
    primeFactorMultiplicityR(n, pItr.next(), pItr, Map())
  }

  def goldbach : List[(Int, Int)] = {
    require(start %2 == 0)

    @tailrec
    def goldbachR(primes : List[Int], found : List[(Int, Int)]) : List[(Int, Int)] = primes match {
      case h :: Nil => found
      case h :: tail => {
        val maydo = tail.dropWhile(_ + h < start)
        if (!maydo.isEmpty && h + maydo.head == start) goldbachR(tail, (h, maydo.head) :: found)
        else goldbachR(tail, found)
      }
      case _ => throw new NoSuchElementException
    }

    goldbachR(S99Int.listPrimesInRange(2 to start), Nil)
  }

  def isPrime: Boolean = S99Int.isPrime(start)

}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

  def listPrimesInRange(r : Range) : List[Int] = (primesStream dropWhile { _ < r.start } takeWhile { _ <= r.last }).toList

  def isPrime(i: Int) =
    if (i == 2) true
    else if ((i & 1) == 0) false // efficient div by 2, @jedws
    else primeCheckStream(i)

  def primesStream: Stream[Int] = 2 #:: primesGenerator

  // performance: avoid redundant divide by two (handled by isPrime), so this starts at 3
  private val primesGenerator: Stream[Int] = {
    def next(i: Int): Stream[Int] =
      if (primeCheckStream(i))
        i #:: next(i + 2)
      else
        next(i + 2) // tail
    3 #:: next(5)
  }

  // assumes not even, check evenness before calling - perf note: must pass partially applied >= method
  private def primeCheckStream(i: Int) =
    primesStream takeWhile (math.sqrt(i).>= _) forall { i % _ != 0 }
}

