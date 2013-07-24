package io.github.abbaspour.s99

import scala.collection.mutable

object P49 {

  // todo: use scalaz memorization
  val memory = mutable.HashMap[Int, List[String]](2 -> List("00", "01", "11", "10"))

  def gray(n : Int) : List[String] = {
    val m = memory.get(n)
    if(m.isDefined)
      m.get
    else {
      val prev = gray(n - 1)
      val mine = prev.map("0" + _) ::: prev.reverse.map("1" + _)
      memory.put(n, mine)
      mine
    }
  }
}
