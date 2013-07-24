package io.github.abbaspour.s99

import scala.collection.immutable.ListSet.ListSetBuilder
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

abstract class GraphBase[T, U] {
  case class Edge(n1: Node, n2: Node, value: U) {
    def toTuple = (n1.value, n2.value, value)
    override def toString = n1.value.toString + "-" + n2.value.toString

    override def equals(p1: Any): Boolean = p1 match {
      case e : Edge => (n1.equals(e.n1) && n2.equals(e.n2)) || (n2.equals(e.n1) && n1.equals(e.n2)) //&& value.equals(e.value)
      case _ => false
    }

    override def hashCode(): Int = 41 * (41 + n1.hashCode()) + n2.hashCode()
  }

  case class Node(value: T) {
    var adj: List[Edge] = Nil
    def neighbors: List[Node] = adj.map(edgeTarget(_, this).get)
    def neighbors(edges : List[Edge]): List[Node] = edges.map(edgeTarget(_, this).getOrElse[Node](this)).distinct

    override def hashCode(): Int = value.hashCode()

    override def equals(n1: Any): Boolean = n1 match {
      case n : Node => value.equals(n.value)
      case _ => false
    }

    def degree: Int = adj.size
  }

  var nodes: Map[T, Node] = Map()
  var edges: List[Edge] = Nil

  // If the edge E connects N to another node, returns the other node,
  // otherwise returns None.
  def edgeTarget(e: Edge, n: Node): Option[Node]

  // http://stackoverflow.com/questions/8136282/graph-isomorphism-algorithm
  def calcInvariant(it : Int) : Map[Node, Int] = {
    val inv : mutable.Map[Node, Int] = mutable.HashMap()
    val onlyNodes = nodes.values
    val size = onlyNodes.size

    inv.sizeHint(size)
    onlyNodes.foreach(n => inv.put(n, n.neighbors.size))

    for(i <- 0 to it) {
      val ninv : mutable.Map[Node, Int] = mutable.HashMap()
      ninv.sizeHint(nodes)
      for (n <- onlyNodes) {
        ninv.put(n, (inv(n) << 13 | inv(n) >> 19) ^ 0xff00ff00)
        for(b <- n.neighbors)
          ninv.put(n, ninv(n) + inv(b))
      }
      for (n <- onlyNodes) {
        inv(n) = ninv(n)
      }
    }

    inv.toMap
  }

  override def equals(o: Any) = o match {
    case g: GraphBase[T,U] => nodes.keys.toSet -- g.nodes.keys.toSet == Nil &&
      edges.map(_.toTuple).toSet -- g.edges.map(_.toTuple) == Nil
    case _ => false
  }

  def addNode(value: T) = {
    val n = new Node(value)
    nodes = Map(value -> n) ++ nodes
    n
  }

  def findPaths(f: T, t: T): List[List[T]] = {
    val visited = mutable.HashSet[Node]()

    def findPathsR(fn : Node, tn: Node, ps : List[T]) : List[List[T]] = {
      val paths : ListBuffer[List[T]] = ListBuffer()

      for {
        nfn <- fn.neighbors
      } {
        if(nfn == tn) paths.+=(ps ::: List(nfn.value))
        else if(!visited.contains(nfn)) {
          visited += nfn
          paths.++=(findPathsR(nfn, tn, ps ::: List(nfn.value)))
        }
      }

      paths.toList
    }

    val fn : Node = nodes.get(f).get
    //visited.+=(fn)
    findPathsR(fn, nodes.get(t).get, List(f))
  }


  def hasPathExcept(e : Edge, edges : List[Edge]): Boolean = {
    val visited = mutable.HashSet[Node]()   // todo: make is map
    visited.sizeHint(this.nodes.size)

    def hasPathExceptR(fn : Node, tn: Node, edges : List[Edge]) : Boolean = {

      for {
        nfn <- fn.neighbors(edges)   // todo: here
      } {
        if(nfn == tn) return true
        else if(!visited.contains(nfn)) {
          visited += nfn
          val cp = hasPathExceptR(nfn, tn, edges)
          if(cp) return true
        }
      }

      false
    }

    hasPathExceptR(e.n1, e.n2, edges)
  }

  def findCycles(n : T) = findPaths(n, n)

  def nodesByDepthFrom(value : T) : List[Node] = {
    val n = nodes.get(value)
    if (!n.isDefined) return Nil
    nodesByDepthFrom(n.get)
  }

  def nodesByDepthFrom(n : Node) : List[Node] = {
    val edges = dfs(n, Nil)
    edges.map(_.n2) ::: n :: Nil
  }

  def dfs : List[Edge] = edges match {
    case h :: tail => dfs(h.n1, Nil)
    case _ => Nil
  }

  def dfs(f : Node, tree : List[Edge]) : List[Edge] = {

    val neighbors = f.neighbors

    for(n <- neighbors) {
      if(! treeHasNode(n, tree)) {
        return dfs(n, tree ::: List(new Edge(f,n,f.adj(0).value)))
      }
    }

    tree
  }

  def allDfs : List[List[Edge]] = edges match {
    case h :: tail => allDfs(h.n1, Nil)
    case _ => Nil
  }

  def treeHasNode(n : Node, tree : List[Edge]) : Boolean = {
    val v = n.value
    for (e <- tree)
      if(e.n1.value == v || e.n2.value == v)
        return true
    false
  }

  def allDfs(f : Node, tree : List[Edge]) : List[List[Edge]] = {
    val bf = ListBuffer[List[Edge]]()

    val neighbors = f.neighbors

    for(n <- neighbors) {
      if(! treeHasNode(n, tree)) {
        val trees = allDfs(n, tree ::: List(new Edge(f,n,f.adj(0).value)))
        bf.++=(trees)
      } else {
        //println("\tskipping node " + n + " already visited")
      }
    }

    val r = bf.toList

    if(r.isEmpty) List(tree) else r
  }

  protected val INV_ITERS = 4

  def isIsomorphicTo(o: GraphBase[T,U]): Boolean = {

    if(this.nodes.size != o.nodes.size) return false

    val mInv = calcInvariant(INV_ITERS)
    val oInv = o.calcInvariant(INV_ITERS)

    def findStartNode(mInv: Map[Node, Int], oInv: Map[o.Node, Int]) : Option[(Node, o.Node)] = {
      for {
        n1 <- mInv
        n2 <- oInv
        if n1._2 == n2._2
      } return Some(n1._1, n2._1)
      None
    }

    val start = findStartNode(mInv, oInv)

    if(!start.isDefined)
      return false

    val (ms, os) = start.get

    val mv = mutable.Set[Node](ms)
    val ov = mutable.Set[o.Node](os)

    val isoMap = mutable.Map[Node, o.Node]()

    def invDfs(ms : Node, os : o.Node) : Boolean = {
      println("starting at " + ms + "/" + os)

      val mbs = ms.neighbors.sortWith(mInv(_) < mInv(_)).filterNot(mv.contains)
      val obs = os.neighbors.sortWith(oInv(_) < oInv(_)).filterNot(ov.contains)

      if(mbs.isEmpty)
        return obs.isEmpty

      for(i <- 0 until mbs.size){
        val nms = mbs(i)
        val nos = obs(i)

        if(mInv(nms) != oInv(nos)) return false

        isoMap(nms) = nos

        mv.+(nms)
        ov.+(nos)

        if(!invDfs(nms, nos)) return false
      }

      println(isoMap.mkString(", "))
      true
    }

    invDfs(ms, os)
  }

  def nodesByDegree : List[Node] = nodes.values.toList.sortBy(_.degree).reverse

  def colorNodes : List[(Node, Int)] = {
    val colored = mutable.HashMap[Node, Int]()
    var c = 1
    var i = 0

    val V = nodesByDegree

    for {
      n <- V
      if !colored.keySet.contains(n)
    } {
      colored(n) = c

      for {
        r <- V.drop(i)
        if n.neighbors.forall(_ != r)
      } {
        colored(r) = c
      }

      c += 1
      i += 1
    }

    colored.toList
  }


  def isBipartite : Boolean = colorNodes.map(_._2).max < 3

  def splitGraph : List[List[Node]] = edges match {
    case h :: tail => splitGraph(h.n1, nodes.values.toSet)
    case _ => Nil
  }

  def splitGraph(n : Node, re : Set[Node]) : List[List[Node]] = {
    val l = nodesByDepthFrom(n)
    val r = re -- l.toSet
    if(r.isEmpty)
      List(l)
    else
       List(l) ::: splitGraph(r.head, r)

  }
}

class Graph[T, U] extends GraphBase[T, U] {
  override def equals(o: Any) = o match {
    case g: Graph[T,U] => super.equals(g)
    case _ => false
  }

  def edgeTarget(e: Edge, n: Node): Option[Node] =
    if (e.n1 == n) Some(e.n2)
    else if (e.n2 == n) Some(e.n1)
    else None

  def addEdge(n1: T, n2: T, value: U) {
    val e = new Edge(nodes(n1), nodes(n2), value)
    edges = e :: edges
    nodes(n1).adj = e :: nodes(n1).adj
    nodes(n2).adj = e :: nodes(n2).adj
  }

  override def toString: String = "[" + edges.mkString(",") + "]"

  def toTermForm: (List[T], List[(T,T,U)]) =
    (nodes.keys.toList, edges.map(e => (e.n1.value, e.n2.value, e.value)))


  def spanningTrees : List[List[Edge]] = spanningTreesR(edges)

  def spanningTreesR(edges : List[Edge]) : List[List[Edge]] = { // todo: flatMap
    val bf = ListBuffer[List[Edge]]()

    for(e <- edges) {
      val ne = edges.filter(_ != e)
      if (hasPathExcept(e, ne))
        bf.++=(spanningTreesR(ne))
    }

    if(bf.isEmpty) bf += edges
    bf.toList.distinct
  }

}


class Digraph[T, U] extends GraphBase[T, U] {

  override def equals(o: Any) = o match {
    case g: Digraph[T,U] => super.equals(g)
    case _ => false
  }

  override def toString: String = "[" + edges.mkString(",") + "]"

  def edgeTarget(e: Edge, n: Node): Option[Node] =
    if (e.n1 == n) Some(e.n2)
    else None

  def addArc(source: T, dest: T, value: U) {
    val e = new Edge(nodes(source), nodes(dest), value)
    edges = e :: edges
    nodes(source).adj = e :: nodes(source).adj
  }

  def toAdjacentForm : (List[T], List[(T,T,U)]) =
    (nodes.keys.toList, edges.map(e => (e.n1.value, e.n2.value, e.value)))
}

abstract class GraphObjBase {
  type GraphClass[T, U]
  def addLabel[T](edges: List[(T, T)]) =
    edges.map(v => (v._1, v._2, ()))
  def term[T](nodes: List[T], edges: List[(T,T)]) =
    termLabel(nodes, addLabel(edges))
  def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]): GraphClass[T, U]
  def addAdjacentLabel[T](nodes: List[(T, List[T])]) =
    nodes.map(a => (a._1, a._2.map((_, ()))))
  def adjacent[T](nodes: List[(T, List[T])]) =
    adjacentLabel(addAdjacentLabel(nodes))
  def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]): GraphClass[T, U]
}

object Graph extends GraphObjBase {
  type GraphClass[T, U] = Graph[T, U]

  def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]) = {
    val g = new Graph[T, U]
    nodes.map(g.addNode)
    edges.map(v => g.addEdge(v._1, v._2, v._3))
    g
  }

  def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]) = {
    val g = new Graph[T, U]
    for ((v, a) <- nodes) g.addNode(v)
    for ((n1, a) <- nodes; (n2, l) <- a) {
      if (!g.nodes(n1).neighbors.contains(g.nodes(n2)))
        g.addEdge(n1, n2, l)
    }
    g
  }

  // todo: use regex
  def splitHumanFriendly(hf : String) : (Set[String], Array[(String, String)]) = {
    val nodes : ListSetBuilder[String] = new ListSetBuilder[String]()
    def mf(s : String) : (String, String) = {
      val i = s.indexOf('-')
      val f = s.take(i)
      val t = s.drop(i+1)
      if(!f.isEmpty) nodes += f
      if(!t.isEmpty) nodes += t
      (f, t)
    }
    val a = hf.slice(1, hf.length - 1).split(',').map(s => mf(s.trim))
    (nodes.result(), a)
  }

  def fromString(s : String) : Graph[String, String] = {
    val (n, a) = splitHumanFriendly(s)
    val edges : Array[(String, String)] = a.flatMap(e => if(!e._1.isEmpty) List((e._1, e._2)) else Nil)
    val g = new Graph[String, String]
    n.map(g.addNode)
    edges.map(v => g.addEdge(v._1, v._2, ""))
    g
  }
}

object Digraph extends GraphObjBase {
  type GraphClass[T, U] = Digraph[T, U]

  def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]) = {
    val g = new Digraph[T, U]
    nodes.map(g.addNode)
    edges.map(v => g.addArc(v._1, v._2, v._3))
    g
  }
  def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]) = {
    val g = new Digraph[T, U]
    for ((n, a) <- nodes) g.addNode(n)
    for ((s, a) <- nodes; (d, l) <- a) g.addArc(s, d, l)
    g
  }

  def splitHumanFriendlyLabeled(hf : String) : (Set[String], Array[(String, String, Int)]) = {
    val nodes : ListSetBuilder[String] = new ListSetBuilder[String]()

    // todo; use regex
    def mf(s : String) : Option[(String, String, Int)] = {
      val i = s.indexOf('>')
      if(i < 1) return None
      val f = s.take(i)
      nodes += f
      val t0 = s.drop(i+1)
      if(t0.isEmpty) return None
      val j = t0.indexOf('/')
      if(j < 1) return None
      val t = t0.take(j)
      if(t.isEmpty) return None
      nodes += t
      val l = t0.drop(j+1)
      Some(f, t, l.toInt)
    }

    val a = hf.slice(1, hf.length - 1).split(',').map(s => mf(s.trim).getOrElse(("", "", 0)))
    (nodes.result(), a)
  }

  def fromStringLabel(s : String) : Digraph[String, Int] = {
    val (n, a) = splitHumanFriendlyLabeled(s)
    val edges : Array[(String, String, Int)] = a.flatMap(e => if(!e._1.isEmpty) List((e._1, e._2, e._3)) else Nil)
    val g = new Digraph[String, Int]
    n.map(g.addNode)
    edges.map(v => g.addArc(v._1, v._2, v._3))
    g
  }
}

