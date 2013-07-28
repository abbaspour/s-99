package io.github.abbaspour.s99

import org.scalatest.FunSuite
import io.github.abbaspour.s99.S99Int._
import io.github.abbaspour.s99.S99Logic._
import io.github.abbaspour.s99.MTree._

import scala.language.implicitConversions
import scala.language.postfixOps
import scala.collection.immutable.ListSet

// todo: ScalaCheck
class S99Suite extends FunSuite {

  // -- Working with lists --
  test("01") {
    assert(8 == P01.lastInList(List(1,1,2,3,5,8)))
  }

  test("02") {
    assert(P02.penultimate(List(1,1,2,3,5,8)) === 5)
  }

  test("03") {
    assert(P03.nth(2, List(1,1,2,3,5,8)) === 2)
  }

  test("04") {
    assert(P04.length(List(1,1,2,3,5,8)) === 6)
  }

  test("05") {
    assert(P05.reverse(List(1,1,2,3,5,8)) === List(8,5,3,2,1,1))
  }

  test("05,2") {
    assert(P05.reverseFold(List(1,1,2,3,5,8)) === List(8,5,3,2,1,1))
  }

  test("06") {
    assert(P06.isPalindrome(List(1,2,3,2,1)) === true)
  }

  test("07") {
    assert(P07.flatten(List(1,1,List(2,2),3,List(4,4),4)) === List(1, 1, 2, 2, 3, 4, 4, 4))
  }

  test("08") {
    assert(P08.compress(List(1,1,2,2,3,4,4,4)) === List(1, 2, 3, 4))
  }

  test("09") {
    assert(P09.pack(List(1,1,2,2,3)) === List(List(1, 1), List(2, 2), List(3)))
  }

  test("09,2") {
    assert(P09.pack(List(1,1,2,2,3,3,3,3,3,4,4)) === List(List(1, 1), List(2, 2), List(3, 3, 3, 3, 3), List(4, 4)))
  }

  test("10") {
    assert(P10.encode(List(1,1,2,2,3,3,3,3,3,4,4)) === List((2,1), (2,2), (5,3), (2,4)))
  }

  test("11") {
    assert(P11.encodeModified(List(1,1,2,3,3,3,3,3,4)) === List((2,1), 2, (5,3), 4))
  }

  test("12") {
    assert(P12.decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) ===
      List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }

  test("13") {
    assert(P13.encodeDirect(List(1,1,2,2,3,3,3,3,3,4)) === List((2,1), (2,2), (5,3), (1,4)))
  }

  test("14") {
    assert(P14.duplicate(List(1,1,2,2,3,3,3,3,3,4)) === List(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4))
  }

  test("15") {
    assert(P15.duplicateN(3, List(1,1,2,2,3,3,3,3,3,4)) ===
      List(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4))
  }

  test("16") {
    assert(P16.drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) === List('b, 'c, 'e, 'f, 'h, 'i, 'k))
  }

  test("17") {
    assert(P17.split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ===
      (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }

  test("18") {
    assert(P18.slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g))
  }

  test("19") {
    val result = P19.rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    assert(result == List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
  }

  test("19-2") {
    val result = P19.rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    assert(result == List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
  }

  test("20") {
    val result = P20.removeAt(1, List('a, 'b, 'c, 'd))
    assert(result == (List('a, 'c, 'd),'b))
  }

  test("21") {
    val result = P21.insertAt('new, 1, List('a, 'b, 'c, 'd))
    assert(result == List('a, 'new, 'b, 'c, 'd))
  }

  test("22") {
    assert(P22.range(4, 9) === List(4, 5, 6, 7, 8, 9))
  }

  test("23") {
    val input : List[Symbol] = List('a, 'b, 'c, 'd, 'f, 'g, 'h)
    val result : List[Symbol] = P23.randomSelect(3, input)
    assert(result.size === 3)
    assert(
      result.forall(e => input.contains(e))
    )
  }

  test("24") {
    val n = 6
    val m = 49
    val result = P24.lotto(n, m)
    assert(n === result.size)
    result forall {
      e => 0 < e && e <= m
    }
  }

  test("25") {
    val input = List('a, 'b, 'c, 'd, 'e, 'f)
    val result = P25.randomPermute(input)

    assert(input.size === result.size)
    assert(input.toSet === result.toSet)
  }

  test("26") {
    val input = List('a, 'b)
    val n = 1
    val output = P26.combinations(n, input)
    //println(output)
    assert(output.size === 2)
  }

  test("26,2") {
    val input = List('a, 'b, 'c, 'd)
    val n = 2
    val output = P26.combinations(n, input)
    //print(output)
    assert(output.size === 6) // 4!/2!x2! = 4x3/2 = 6
  }

  test("26C") {
    val input = List('a, 'b)
    val n = 2
    val output = P26.perms(n, input)
    //println(output.mkString("\n"))
    assert(output.size === 2)
  }

  test("26C,2") {
    val input = List('a, 'b, 'c, 'd, 'e, 'f)
    val n = 3
    val output = P26.perms(n, input)
    //print(output.mkString("\n"))
    assert(output.size === 120) // 6!/3! = 6x5x4 = 120
  }

  test("26C,3") {
    val input = List('a, 'b, 'c, 'd)
    val n = 2
    val output = P26.perms(n, input)
    //print(output.mkString("\n"))
    assert(output.size === 12) // 4!/2! = 4x3 = 12
  }

  test("27") {
    val out = P27.group(List(2, 3, 4), Set("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
    //println(out.mkString("\n"))
    assert(out.size === 1260)
  }

  test("28") {
    val input = List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))
    val output = P28.lsort(input)
    val expected = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))
    assert(output === expected)
  }

  // -- Arithmetic --
  test("31") {
    assert(7.isPrime)
  }

  test("31,1") {
    assert(
      91.isPrime === false
    )
  }

  test("31,3") {
    assert(
      91.isPrime === false
    )
  }

  test("31,4") {
    assert(25.isPrime === false)
  }

  test("32") {
    assert(P32.gcd(36, 63) === 9)
    assert(P32.gcd(63, 36) === 9)
  }

  test("33") {
    assert(35.isCoprimeTo(64))
  }

  test("34") {
    assert(10.totient === 4)
  }

  test("35") {
    assert(315.primeFactors === List(3, 3, 5, 7))
  }

  test("36") {
    assert(315.primeFactorMultiplicity === Map(3 -> 2, 5 -> 1, 7 -> 1))
  }

  test("37") {
    assert(P37.phi(10) === 4)
  }

  test("37,2") {
    assert(P37.phi(315) === 144)
  }

  test("38") {
    val now1 = System.currentTimeMillis
    val phi1 = 10090.totient
    val t1 = System.currentTimeMillis() - now1

    val now2 = System.currentTimeMillis
    val phi2 = P37.phi(10090)
    val t2 = System.currentTimeMillis() - now2

    assert(phi1 === phi2)
    println(s"phi1 calculated in $t1 ms = $phi1")
    println(s"phi2 calculated in $t2 ms = $phi2")
  }

  test("39") {
    assert(S99Int.listPrimesInRange(7 to 31) === List(7, 11, 13, 17, 19, 23, 29, 31))
  }

  test("39,1") {
    assert(S99Int.listPrimesInRange(2 to 31) === List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31))
  }


  test("40") {
    assert(28.goldbach === List((11,17), (5,23)))
  }

  // -- Logic and Codes --
  test("46") {
    assert(not(a = true) === false)
  }

  test("47") {
    table2((a: Boolean, b: Boolean) => and(a, or(a, b)))
  }

  test("49") {
    assert(P49.gray(3) ===  List("000", "001", "011", "010", "110", "111", "101", "100"))
  }


  test("50") {
    val output = P50.huffman(List(('a', 45), ('b', 13), ('c', 12), ('d', 16), ('e', 9), ('f', 5)))
    println(output)
    val expected = List(('a',"0"), ('c',"100"), ('b',"101"), ('d',"111"), ('f',"1100"), ('e',"1101"))
    println(expected)
    assert(
      output === expected
    )
  }

  // -- Binary Trees --
  test("55") {
    val t = Tree.cBalanced(4, "x")
    //println("result  s: " + t)
    assert(t === Node("x", Node("x", Node("x"), End), Node("x")))
  }

  test("56") {
    assert(Node('a', Node('b'), Node('c')).isSymmetric === true)
  }

  test("56,1") {
    assert(Node('a', Node('b'), Node('c', Node('e'), End)).isSymmetric === false)
  }

  test("57") {
    val res0 = End.addValue(2)
    assert(res0 === Node(2, End, End))

    val res1 = res0.addValue(3)
    assert(res1 === Node(2, End, Node(3, End, End)))

    val res2 = res1.addValue(0)
    assert(res2 === Node(2, Node(0, End, End), Node(3, End, End)))
  }

  test("57,2") {
    val tree = Tree.fromList(List(3, 2, 5, 7, 1))
    assert(tree ===
      Node(3,
        Node(2,
          Node(1, End, End),
          End),
        Node(5,
          End,
          Node(7, End, End)
        )
      )
    )
  }

  test("57,3") {
    assert(Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric === true)
  }

  test("57,4") {
    assert(Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric === false)
  }

  test("58") {
    val result = Tree.symmetricBalancedTrees(3, "x")
    //for(r <- result) println(r)
    assert(result.size === 1)
  }

  test("58,2") {
    val result = Tree.symmetricBalancedTrees(5, "x")
    //for(r <- result) println(r)
    assert(result.size === 2)
  }

  test("59") {
    val result = Tree.hbalTrees(3, "x")
    //println(result.mkString("\n"))
    assert(result.size === 8)
  }

  test("60") {
    assert(Tree.minHbalNodes(2) === 2)
    assert(Tree.minHbalNodes(3) === 4)
  }

  test("60,1") {
    assert(Tree.maxHbalHeight(2) === 2)
    assert(Tree.maxHbalHeight(4) === 3)
  }

  test("60,2") {
    val result = Tree.hbalTreesWithNodes(4, "x")
    //println(result.mkString("\n"))
    assert(result.size === 2)
  }

  test("60,3") {
    val result = Tree.hbalTreesWithNodes(15, "x")
    val c = result.size
    assert(c === 1)
  }

  test("61") {
    assert(Node('x', Node('x'), End).leafCount === 1)
  }

  test("61A") {
    assert(Node('a', Node('b'), Node('c', Node('d'), Node('e'))).leafList === List('b', 'd', 'e'))
  }

  test("62") {
    assert(Node('a', Node('b'), Node('c', Node('d'), Node('e'))).internalList === List('a', 'c'))
  }

  test("62B") {
    assert(Node('a', Node('b'), Node('c', Node('d'), Node('e'))).atLevel(2) === List('b', 'c'))
  }

  test("63") {
    assert(Tree.completeBinaryTree(3, "x") === Node("x", Node("x"), Node("x")))
  }

  test("63,1") {
    assert(Tree.completeBinaryTree(4, "x") === Node("x", Node("x", Node("x"), End), Node("x")))
  }

  test("63,2") {
    assert(Tree.completeBinaryTree(6, "x") === Node("x", Node("x", Node("x"), Node("x")), Node("x", Node("x"), End)))
  }

  test("64") {
    val t = Node('a', Node('b', End, Node('c')), Node('d'))
    val r = t.layoutBinaryTree
    //println(r)
    assert(r.toString === "T[3,1](a T[1,2](b  T[2,3](c  )) T[4,2](d  ))")
  }

  test("64,2") {
    val t = Tree.fromList(List('n','k','m','c','a','h','g','e','u','p','s','q'))
    val r = t.layoutBinaryTree
    //println(r)
    assert(r.toString === "T[8,1](n T[6,2](k T[2,3](c T[1,4](a  ) T[5,4](h T[4,5](g T[3,6](e  ) ) )) T[7,3](m  )) T[12,2](u T[9,3](p  T[11,4](s T[10,5](q  ) )) ))");
  }

  test("65") {
    val t = Tree.fromList(List('n','k','m','c','a','h','g','e','u','p','s','q'))
    //println(t)
    assert(t.toString === "n(k(c(a,h(g(e,),)),m),u(p(,s(q,)),))")
  }

  test("66") {
    // todo: impl me plz
    //assert(condition = false)
  }

  test("67") {
    val s = Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End))).toString
    assert(s === "a(b(d,e),c(,f(g,)))")
  }

  test("67, 1") {
    val t = Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End)))
    val t2 = Tree.fromString(t.toString)
    assert(t2.toString === t.toString)
    assert(t2 === t)
  }

  test("68") {
    assert(Tree.fromString("a(b(d,e),c(,f(g,)))").preorder === List('a', 'b', 'd', 'e', 'c', 'f', 'g'))
  }

  test("68,2") {
    assert(Tree.fromString("a(b(d,e),c(,f(g,)))").inorder === List('d', 'b', 'e', 'a', 'c', 'g', 'f'))
  }

  test("68,3") {
    val t = Tree.preInTree(List('a', 'b', 'd', 'e', 'c', 'f', 'g'), List('d', 'b', 'e', 'a', 'c', 'g', 'f'))
    //println(t)
    assert(t.toString === "a(b(d,e),c(,g))")
  }

  test("68,3,2") {
    val t = Tree.preInTree(List('c', 'f', 'g'), List('c', 'g', 'f'))
    //println(t)
    assert(t.toString === "c(,g)")
  }

  test("69") {
    val t = Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End)))
    //println(t.toDotstring)
    assert(t.toDotstring === "abd..e..c.fg...")
  }

  test("69,2") {
    //val t = Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End)))
    val t  = Tree.fromDotstring("abd..e..c.fg...")
    //println(t)
    assert(t.toString === "a(b(d,e),c(,f(g,)))")
  }


  // -- Multiway Trees --
  test("70C") {
    assert(MTree('a', List(MTree('f'))).nodeCount === 2)
  }

  test("70") {
    val s = MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).toString
    assert(s === "afg^^c^bd^e^^^")
  }

  test("70,splitAtCarrot") {
    assert(MTree.splitAtCarrot("fg^^c^bd^e^^") === List("fg^", "c", "bd^e^"))
  }

  test("70,2") {
    val mt = MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e')))))
    assert(MTree.string2MTree("afg^^c^bd^e^^").toString === mt.toString) // todo: ^^^
  }

  test("70,3") {
    assert("afg^^c^bd^e^^".nodeCount === 7)
  }

  test("71") {
    assert("afg^^c^bd^e^^".internalPathLength === 10) // todo: 9 or 10?
  }

  test("72") {
    assert("afg^^c^bd^e^^".postorder === List('g', 'f', 'c', 'd', 'e', 'b', 'a'))
  }

  test("73") {
    assert(MTree("a", List(MTree("b", List(MTree("c"))))).lispyTree === "(a (b c))")
  }

  test("73,1") {
    assert(MTree.splitAtParantesis("a (f g)") === List("a", "(f g)"))
  }

  test("73,2") {
    val t = MTree.fromLispy("(a (b c))")
    assert(t.lispyTree === "(a (b c))")
  }

  test("73,3") {
    val s = "(a (f g) c (b d e))"
    val t = MTree.fromLispy(s)
    assert(t.lispyTree === s)
  }

  // -- Graph --
  test("80,0") {
    val g = Graph.term(List('b', 'c', 'd', 'f', 'g', 'h', 'k'),
      List(('b', 'c'), ('b', 'f'), ('c', 'f'), ('f', 'k'), ('g', 'h')))
    assert(g.toString === "[g-h,f-k,c-f,b-f,b-c]")
  }

  test("80,1") {
    val hf = "[b-c, f-c, g-h, d, f-b, k-f, h-g]"
    val (nodes, a) = Graph.splitHumanFriendly(hf)
    assert(nodes === ListSet("b", "c", "f", "g", "h", "d", "k"))
    assert(a ===
      List(
        ("b", "c"), ("f", "c"), ("g", "h"), ("", "d"), ("f", "b"), ("k", "f"), ("h", "g")
    ).toArray
    )
  }

  test("80") {
    val g = Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]")
    assert(g.toString === "[h-g,k-f,f-b,g-h,f-c,b-c]")
  }

  test("80,2") {
    val g = Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]")
    val g2 = g.toTermForm
    //println(g2)
    assert(g2._1 === List("f", "b", "g", "c", "h", "k", "d"))
    assert(g2._2 === List(("h","g",""), ("k","f",""), ("f","b",""), ("g","h",""), ("f","c",""), ("b","c","")))
  }

  test("80,3") {
    val (n, edges : Array[(String, String, Int)]) = Digraph.splitHumanFriendlyLabeled("[p>q/9, m>q/7, k, p>m/5]")
    assert(n === ListSet("m", "q", "p"))
    //println(edges.mkString(" & "))
    assert(edges.mkString(" & ") === "(p,q,9) & (m,q,7) & (,,0) & (p,m,5)")
  }

  test("80,4") {
    val g = Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]")
    //println(g.toAdjacentForm)
    assert(g.toAdjacentForm.toString() === "(List(p, q, m),List((p,m,5), (m,q,7), (p,q,9)))")
  }

  test("81") {
    val p = Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").findPaths("p", "q")
    assert(p === List( List("p", "m", "q"), List("p", "q")))
  }

  // todo: fails, fix 'k'
  test("81,2") {
    val p = Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").findPaths("p", "k")
    assert(p === List())
  }

  // todo: fails, why?
  test("82") {
    val p = Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").findCycles("f")
    assert(p === List(List("f", "c", "b", "f"), List("f", "b", "c", "f")))
  }

  test("83,1") {
    val g = Graph.fromString("[a-b, b-c, a-c]")
    val d = g.allDfs
    //println(d)
    assert(d.toString() === "List(List(a-c, c-b), List(a-b, b-c))")
  }

  test("83") {
    val g =  Graph.fromString("[a-b, b-c, a-c]")
    val st = g.spanningTrees
    //println("number of spanning trees: " + st.size)
    //println(st.mkString("\n"))
    assert(st.size === 3)
  }

  test("83,2") {
    val g =  Graph.fromString("[a-b, b-c, c-d, d-a]")
    val st = g.spanningTrees
    println("number of spanning trees: " + st.size)
    assert(st.size === 4)
    println(st.mkString("\n"))
  }


  test("85,0") {
    val g =  Graph.fromString("[a-b, b-c, c-d, d-a]")
    val i0 = g.calcInvariant(0)
    println("inv in itr 0 = " + i0.mkString(","))
    val i1 = g.calcInvariant(1)
    println("inv in itr 0 = " + i1.mkString(","))
    val i2 = g.calcInvariant(2)
    println("inv in itr 0 = " + i2.mkString(","))
  }

  test("85,1") {
    val g =  Graph.fromString("[a-b, b-c, c-d, d-a, a-c]")
    val i0 = g.calcInvariant(0)
    println("inv in itr 0 = " + i0.mkString(","))
    val i1 = g.calcInvariant(1)
    println("inv in itr 0 = " + i1.mkString(","))
    val i2 = g.calcInvariant(2)
    println("inv in itr 0 = " + i2.mkString(","))
  }

  test("85") {
    val g1 = Graph.fromString("[a-b]")
    val g2 = Graph.fromString("[5-7]")

    assert(g1.isIsomorphicTo(g2))
  }

  test("86,a") {
    assert(Graph.fromString("[a-b, b-c, a-c, a-d]").nodes("a").degree === 3)
  }

  test("86,b") {
    val l = Graph.fromString("[a-b, b-c, a-c, a-d]").nodesByDegree
    //println(l.mkString(","))
    assert(l.mkString(",") === "Node(a),Node(c),Node(b),Node(d)")
  }

  test("86") {
    val l = Graph.fromString("[a-b, b-c, a-c, a-d]").colorNodes
    //println(l.mkString(","))
    assert(l.mkString(",") === "(Node(b),3),(Node(d),3),(Node(a),1),(Node(c),2)")
  }

  test("87") {
    val l = Graph.fromString("[a-b,c]").dfs
    assert(l.mkString(",") === "a-b")
  }

  test("87,2") {
    val l2 = Graph.fromString("[a-b, b-c, e, a-c, a-d]").nodesByDepthFrom("d")
    assert(l2.mkString(",") === "Node(a),Node(c),Node(b),Node(d)")
  }

  test("88") {
    val l = Graph.fromString("[a-b,c]").splitGraph
    assert(l.mkString(",") === "List(Node(b), Node(a)),List(Node(c))")
  }

  test("89") {
    assert(Graph.fromString("[a-b, b-c, c-a]").isBipartite === false)
  }

  test("89,1") {
    assert(Graph.fromString("[a-b, b-c, d]").isBipartite === true)
  }

  test("89,2") { // todo: why fails
    assert(Graph.fromString("[a-b, b-c, d, e-f, f-g, g-e, h]").isBipartite === true)
  }

  // -- Miscellaneous Problems --
  test("90") {
    val r = P90.rec_n_queue(8)
    assert(r.size === 92)
  }

  // todo: impl 92 .. 99
  test("92") {
    val g = Graph.fromString("[a-b, b-c, c-d]")
    //g.vonKoch
  }
}
