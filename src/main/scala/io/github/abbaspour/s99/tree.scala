package io.github.abbaspour.s99 {

import scala.collection.mutable.ListBuffer

sealed abstract class Tree[+T] {
  def isMirrorOf[A](that: Tree[A]) : Boolean
  def addValue[U >: T <% Ordered[U]](x: U): Tree[U]
  def addValue[U >: T <% Ordered[U]](x: U, tendLeft : Boolean): Tree[U]
  def isSymmetric : Boolean
  def leafCount : Int
  def leafList: List[T]
  def internalList: List[T]
  def atLevel(l : Int) : List[T]
  def treeDepth: Int
  def layoutBinaryTree: Tree[T] = layoutBinaryTreeInternal(1, 1)._1
  def layoutBinaryTreeInternal(x: Int, depth: Int): (Tree[T], Int)
  def preorder : List[T]
  def inorder : List[T]
  def toDotstring : String
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  //override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
  override def toString =
    if(left == End && right == End) value.toString
    else value + "(" + left.toString + "," + right.toString + ")"

  override def toDotstring =
      value + left.toDotstring + right.toDotstring

  override def isSymmetric = left.isMirrorOf(right)

  override def addValue[U >: T <% Ordered[U]](x: U): Tree[U] =
    if (x < value) Node(value, left.addValue(x), right)
    else Node(value, left, right.addValue(x))

  override def addValue[U >: T <% Ordered[U]](x: U, tendLeft : Boolean): Tree[U] =
    if (x < value) Node(value, left.addValue(x), right)
    else if(x > value) Node(value, left, right.addValue(x))
    else if(tendLeft) Node(value, left.addValue(x), right)
    else Node(value, left, right.addValue(x))

  override def isMirrorOf[A](that: Tree[A]) : Boolean = that match {
    case thatNode : Node[A] => left.isMirrorOf(thatNode.right) && right.isMirrorOf(thatNode.left)
    case End => false
  }

  override def leafCount : Int =
    if (right == End && left == End) 1
    else  (if(left != End) left.leafCount else 0) + (if (right != End) right.leafCount else 0)

  def leafList: List[T] =
    if (right == End && left == End) List(value)
    else left.leafList ::: right.leafList

  def internalList: List[T] =
    if (right == End && left == End) Nil
    else value :: left.internalList ::: right.internalList

  def atLevel(l: Int): List[T] =
    if (l < 1) throw new NoSuchFieldException
    else if (l == 1) List(value)
    else left.atLevel(l - 1) ::: right.atLevel(l - 1)

  def layoutBinaryTreeInternal(x: Int, depth: Int): (Tree[T], Int) = {
    val (leftTree, myX) = left.layoutBinaryTreeInternal(x, depth + 1)
    val (rightTree, nextX) = right.layoutBinaryTreeInternal(myX + 1, depth + 1)
    (new PositionedNode(value, leftTree, rightTree, myX, depth), nextX)
  }

  def treeDepth: Int = Math.max(left.treeDepth, right.treeDepth) + 1

  override def preorder: List[T] = value :: left.preorder ::: right.preorder

  override def inorder: List[T] = left.inorder ::: value :: right.inorder
}

case object End extends Tree[Nothing] {
  //override def toString = "."
  override def toString = ""
  override def toDotstring = "."
  override def isSymmetric = true
  override def addValue[U <% Ordered[U]](x: U): Tree[U] = Node(x)
  override def addValue[U <% Ordered[U]](x: U, tendLeft : Boolean) = Node(x)
  override def leafCount = 0

  override def isMirrorOf[A](that: Tree[A]) : Boolean = that match {
    case _ : Node[A] => false
    case End => true
  }

  def leafList = Nil

  def internalList = Nil

  def atLevel(l : Int) = Nil

  def layoutBinaryTreeInternal(x: Int, depth: Int) = (End, x)

  def treeDepth: Int = 0

  override def preorder = Nil

  override def inorder = Nil
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

class PositionedNode[+T](override val value: T, override val left: Tree[T], override val right: Tree[T], val x: Int, val y: Int) extends Node[T](value, left, right) {
  override def toString = "T[" + x.toString + "," + y.toString + "](" + value.toString + " " + left.toString + " " + right.toString + ")"
}

object Tree {
  def hbalTrees[T] (h: Int, a: T) : Seq[Tree[T]] = {
    if (h == 0) return List(Node(a))
    if (h == 1) return List(Node(a, Node(a), End), Node(a, End, Node(a)))
    val half = h / 2
    val sameH = h % 2 == 0
    val (i, j) = if (sameH) (half, half) else (half, half + 1)

    val subTreeIs = hbalTrees(i, a)
    val subTreeJs = if(sameH) subTreeIs else hbalTrees(j, a)

    if(!sameH)
      for {
        si <- subTreeIs
        sj <- subTreeJs
      } yield Node(a, sj, si)

    for {
      si <- subTreeIs
      sj <- subTreeJs
    } yield Node(a, si, sj)
  }

  def hbalTreesWithNodes[T] (n: Int, a: T) : Seq[Tree[T]] = {
    if (n == 0) return List(End)
    if (n == 1) return List(Node(a))
    if (n == 2) return List(Node(a, Node(a), End), Node(a, End, Node(a)))

    val half = (n - 1) / 2
    val sameH = (n - 1) % 2 == 0
    val (i, j) = if (sameH) (half, half) else (half, half + 1)

    val subTreeIs = hbalTreesWithNodes(i, a)
    val subTreeJs = if(sameH) subTreeIs else hbalTreesWithNodes(j, a)

    if(!sameH)
      for {
        si <- subTreeIs
        sj <- subTreeJs
      } yield Node(a, sj, si)

    for {
      si <- subTreeIs
      sj <- subTreeJs
    } yield Node(a, si, sj)
  }

  def minHbalNodes(h: Int) : Int = {
    if (h == 0) return 0
    if (h == 1) return 1
    if (h == 2) return 2
    val half = h / 2
    val sameH = h % 2 == 0
    val (i, j) = if (sameH) (half, half) else (half, half + 1)

    val subTreeIs = minHbalNodes(i)
    val subTreeJs = if(sameH) subTreeIs else minHbalNodes(j)

    1 + subTreeIs + subTreeJs
  }

  def maxHbalHeight(n : Int) : Int = {
    if (n == 0) return 0
    if (n == 1) return 1
    if (n == 2) return 2
    val half = (n - 1) / 2
    if ((n - 1) % 2 == 0) maxHbalHeight(half) + 1 else maxHbalHeight(half + 1) + 1
  }

  def cBalanced[T](n : Int, e : T) : Tree[T] = {
    if(n == 1) return Node(e)
    if(n == 0) return End
    val rc = (n - 1) / 2
    val lc = if ((n - 1) % 2 == 0) rc else rc + 1
    Node(e, cBalanced(lc, e), cBalanced(rc, e))
  }

  def fromList[T <% Ordered[T]](xs : List[T]) : Tree[T] =
    xs.foldLeft(End:Tree[T])((t: Tree[T], x : T) => t.addValue(x))

  def bits(i : Int, n : Int) : Array[Boolean] = {
    val chars = i.toBinaryString.toCharArray
    val res = new Array[Boolean](n)
    var d = 0

    while(d < n - chars.length) {
      res(d) = false
      d += 1
    }

    for (c <- chars) {
      res(d) = c == '1'
      d += 1
    }

    res
  }

  def membershipItr(n : Int) : Seq[Array[Boolean]] =
    for (i <- 0 to Math.pow(2,n - 1).toInt)
      yield bits(i, n)

  def generateTree[A](n : Int, a : A) : List[Tree[A]] = {
    if (n == 0) return List(End)
    if (n == 1) return List(Node(a))
    var i = n - 1 // on left
    var j = 0     // on right
    val myTrees : ListBuffer[Tree[A]] = ListBuffer[Tree[A]]()
    while(j < n) {
      val onLeft = generateTree(i, a)
      val onRight = generateTree(j, a)
      for (l <- onLeft)
        for(r <- onRight)
          myTrees.+=(Node(a, l, r))
      i -= 1
      j += 1
    }
    myTrees.toList
  }

  def generateTreeSeq[A](n : Int, a : A) : Seq[Tree[A]] = {
    if (n == 0) return List(End)
    if (n == 1) return List(Node(a))
    var i = n - 1 // on left
    var j = 0     // on right
    while(j < n) {
      val onLeft = generateTreeSeq(i, a)
      val onRight = generateTreeSeq(j, a)
      for (l <- onLeft)
        for(r <- onRight)
          yield Node(a, l, r)
      i -= 1
      j += 1
    }
    Nil
  }

  def symmetricBalancedTrees[A <% Ordered[A]](n : Int, e : A) : Seq[Tree[A]] =
    for(t <- generateTree(n, e) if t.isSymmetric)
      yield t

  def completeBinaryTree[A](i: Int, n : Int, e : A) : Tree[A] = {
    val left = if (i * 2 <= n) completeBinaryTree(i * 2, n, e)
    else End
    val right = if (i * 2 + 1<= n) completeBinaryTree(i * 2 + 1, n, e)
    else End

    Node(e, left, right)
  }

  def completeBinaryTree[A](n : Int, e : A) : Tree[A] =
    if (n == 0) End
    else completeBinaryTree(1, n, e)

  def bfs[A](tree : Tree[A], f: A => Unit) {

    if(tree == End) return
    var list : List[Node[A]] =  List[Node[A]](tree.asInstanceOf[Node[A]])

    while(! list.isEmpty) {
      val (h, tail) = list.splitAt(1)
      f(h.head.value)
      val l : List[Node[A]] = if (h.head.left == End) Nil else List(h.head.left.asInstanceOf[Node[A]])
      val r : List[Node[A]] = if (h.head.right == End) Nil else List(h.head.right.asInstanceOf[Node[A]])
      list = tail ::: l ::: r
    }
  }

  def fromString(input : String) : Tree[Char] = {
      val s = input.dropWhile(c => c == '(' || c == ')')
      if(s.isEmpty) return End
      val value = s.charAt(0)
      if(s.length == 1 || s.charAt(1) != '(') return Node(value)
      var par = 0
      val (left, right) = s.drop(2).span(
        c =>
          if (c == '(') { par += 1; true}
          else if(c == ')') { par -= 1; true}
          else if(c == ',' && par == 0) false
          else true
      )
      Node(value, fromString(left), fromString(right.drop(1)))
    }

  def fromDotstring(s : String) : Tree[Char] = fromDotstringInternal(s)._1

  def fromDotstringInternal(s : String) : (Tree[Char], String) = {
    if(s.isEmpty) return (End, "")
    val value = s.charAt(0)
    if(value == '.') return (End, s.drop(1))
    if (s.length == 1) return (Node(value), "")
    val leftVal = s.charAt(1)
    val (leftTree, rightString) = if(leftVal == '.') (End, s.drop(2)) else fromDotstringInternal(s.drop(1))
    val (rightTree, restString) = fromDotstringInternal(rightString)
    (Node(value, leftTree, rightTree), restString)
  }

  def preInTree[A <% Ordered[A]](preoder : List[A], inorder : List[A]) : Tree[A] = {
    if(preoder.isEmpty) return End
    val value = preoder.head
    val rest = preoder.tail
    if(rest.isEmpty) return Node(value)
    val (leftInorder, rightValueInorder) = inorder.span(_ != value)
    val rightInorder = rightValueInorder.tail
    val rightValue = rightInorder.head


    val (leftPreorder, rightPreorfer) = rest.span(_ != rightValue)
    Node(value,
      if (leftInorder.isEmpty) End else preInTree(leftPreorder, leftInorder),
      if (rightInorder.isEmpty) End else preInTree(rightPreorfer, rightInorder)
    )
  }
}
}