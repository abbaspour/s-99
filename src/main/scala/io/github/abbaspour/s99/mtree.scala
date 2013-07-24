package io.github.abbaspour.s99

import scala.collection.mutable.ListBuffer

class MTree[+T](value: T, children: List[MTree[T]]) {
  def this(value: T) = this(value, List())
  //override def toString = "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"
  override def toString = children.foldLeft(value.toString)(_ + _.toString) + "^"
  def nodeCount : Int = children.foldLeft(1)(_ + _.nodeCount)
  def internalPathLength : Int = internalPathLengthInternal._1
  private def internalPathLengthInternal : (Int, Int) =
    (
      children.foldLeft(1)(
        (z,c) => z + c.internalPathLengthInternal._1 + c.internalPathLengthInternal._2
      ),
      children.size
    )

  def postorder : List[T] = children.flatMap(c => c.postorder) ::: List(value)

  def lispyTree : String =
    if(children.isEmpty) value.toString
    else "(" + value.toString + children.foldLeft("")(_ + " " + _.lispyTree) + ")"

}

object MTree {
  def apply[T](value: T) = new MTree(value, List())
  def apply[T](value: T, children: List[MTree[T]]) = new MTree(value, children)

  def splitAtCarrot(s : String) : List[String] = {
    if(s.isEmpty) return Nil
    val lb = new ListBuffer[String]()
    var t = s
    var depth = 0
    while (!t.isEmpty) {
      var lc = t.charAt(0)
      val (h,nt) = t.span {
        c =>
          depth += (if (c == '^') -1 else 1)
          if (lc == '^' && c != '^' && depth == 1) false
          else {lc = c; true}
      }
      depth = 0
      lb += h.dropRight(1)
      t = nt
    }
    lb.toList
  }

  def splitAtParantesis(s : String) : List[String] = {
    if(s.isEmpty) return Nil
    val lb = new ListBuffer[String]()
    var t = s
    var depth = 0
    while (!t.isEmpty) {
      val (h,nt) = t.span {
        c =>
          depth += (if (c == '(') 1 else if (c == ')') -1 else 0)
          !(c == ' ' && depth == 0)
      }
      lb += h.trim
      t = nt.trim
    }
    lb.toList
  }

  implicit def string2MTree(s : String) : MTree[Char] = {
    if(s.isEmpty) throw new NoSuchElementException
    val value = s.charAt(0)
    val childS = s.drop(1)
    if(childS.isEmpty) return MTree(value)
    val splitedChildS = splitAtCarrot(childS)
    val childs : List[MTree[Char]] = splitedChildS.map(c => MTree.string2MTree(c))
    MTree(value, childs)
  }

  def fromLispy(s: String) : MTree[Char] =
    if (s.startsWith("(")) {
      val ns = s.drop(3)
      val value = s.charAt(1)
      val l = splitAtParantesis(ns)
      val childs = l.map(fromLispy(_))
      MTree(value, childs)
    } else MTree(s.charAt(0))

}