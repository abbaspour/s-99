package io.github.abbaspour.s99

import scala.collection.mutable

object P50 {

  //todo: simplify, return byte not string
  def huffman(input : List[(Char, Int)]) : List[(Char, String)] = {

    val charMap : mutable.HashMap[Char, String] = mutable.HashMap[Char, String]()
    input.foreach(e => charMap.put(e._1, ""))
    var joined : mutable.LinkedList[(String, Int)] = mutable.LinkedList[(String, Int)]()
    input.foreach(e => joined = joined:+(e._1.toString, e._2))
    joined = joined.sortBy(_._2)

    while (joined.size > 1) {
      val (firsttwo, rest) = joined.splitAt(2)
      val first = firsttwo.get(0).get
      val second = firsttwo.get(1).get

      joined = rest:+(first._1 + second._1, first._2 + second._2)
      joined = joined.sortBy(_._2)

      first._1.foreach(c => charMap.put(c, "0" + charMap.get(c).get))
      second._1.foreach(c => charMap.put(c, "1" + charMap.get(c).get ))
    }

    //println(sorted)
    //println(charMap)

    charMap.toList.sortBy(_._2.toInt)
  }
}
