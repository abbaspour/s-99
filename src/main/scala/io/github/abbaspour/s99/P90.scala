package io.github.abbaspour.s99

import scala.collection.mutable.ListBuffer

object P90 {
  def last_one_hits (board: List[Int], pos : Int) : Boolean = {
    if (board.isEmpty) return false
    val x = pos
    val i = board.length

    var j = 0

    board.foreach(y => {
      if (j == i) return false
      if (x == y) return true
      val d = x - y
      if (i - j == d || j - i == d)
        return true
      j += 1
    })

    false
  }

  def inRange(b : List[Int], p : Int, min : Int, max : Int) = !b.isEmpty || (b.isEmpty && (min <= p && p < max))

  def rec_n_queue(n : Int) : List[Array[Int]] = {
    val answers = new ListBuffer[Array[Int]]
    val r1 = inRange(_ : List [Int], _ : Int, 0, n/2)
    val r2 = inRange(_ : List [Int], _ : Int, n/2, n)
    rec_n_queue(n, List(), -1, r1, answers)
    rec_n_queue(n, List(), (n/2) - 1, r2, answers)
    answers.toList
  }


  def rec_n_queue(n : Int, board : List[Int], i: Int,  myrange: (List[Int], Int) => Boolean, answers : ListBuffer[Array[Int]]) {
    var pos = i + 1

    if (!myrange(board, pos)) return

    val new_board = if (pos == n) {
      pos = board.last
      board.init
    } else {
      if (!last_one_hits(board, pos)) {
        if (board.length == n - 1) {
          answers += (board ::: List(pos)).toArray
          board
        } else {
          val b = board ::: List(pos)
          pos = -1
          b
        }
      } else {
        board
      }
    }

    rec_n_queue(n, new_board, pos, myrange, answers)
  }

}
