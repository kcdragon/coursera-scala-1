package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of tree with one element") {
    val t0 = Leaf('a', 1)
    assert(weight(t0) === 1)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a tree with one element") {
    val t0 = Leaf('a', 1)
    assert(chars(t0) === List('a'))
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times: empty list") {
    val l = List()
    assert(times(l) === List())
  }

  test("times: list with one letter and single occurence") {
    val l = List('a')
    assert(times(l) === List(('a', 1)))
  }

  test("times: list with multiple letters and all with single occurences") {
    val l = List('a', 'b')
    assert(times(l) === List(('a', 1), ('b', 1)))
  }

  test("times: list with one letter and multiple occurences") {
    val l = List('a', 'a')
    assert(times(l) === List(('a', 2)))
  }

  test("times: list with multiple letters with multiple occurences") {
    val l = List('a', 'b', 'a')
    assert(times(l) === List(('a', 2), ('b', 1)))
  }

  test("makeOrderedLeafList: empty list") {
    val l = List()
    assert(makeOrderedLeafList(l) === List())
  }

  test("makeOrderedLeafList: single element list") {
    val l = List(('a', 1))
    assert(makeOrderedLeafList(l) === List(Leaf('a', 1)))
  }

  test("makeOrderedLeafList: frequency list is already sorted") {
    val l = List(('a', 1), ('b', 2))
    assert(makeOrderedLeafList(l) === List(Leaf('a', 1), Leaf('b', 2)))
  }

  test("makeOrderedLeafList: frequency list is not sorted") {
    val l = List(('a', 2), ('b', 1))
    assert(makeOrderedLeafList(l) === List(Leaf('b', 1), Leaf('a', 2)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton: empty list is false") {
    val l = List()
    assert(!singleton(l))
  }

  test("singleton: single element list is true") {
    val l = List(Leaf('a', 1))
    assert(singleton(l))
  }

  test("singleton: multi element list is false") {
    val l = List(Leaf('a', 1), Leaf('b', 1))
    assert(!singleton(l))
  }

  test("combine: empty list") {
    val l = List()
    assert(combine(l) === l)
  }

  test("combine: single element list") {
    val l = List(Leaf('a', 1))
    assert(combine(l) === l)
  }

  test("combine: two element list") {
    val l = List(Leaf('a', 1), Leaf('b', 1))
    assert(combine(l) === l)
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine: fork and leaf list") {
    val fork = Fork(Leaf('t', 2), Leaf('s', 3), List('t', 's'), 5)
    val leaflist = List(Leaf('e', 1), fork, Leaf('x', 4))
    assert(combine(leaflist) === List(Leaf('x',4), Fork(Leaf('e',1),fork,List('e', 't', 's'),6)))
  }

  test("until: combines two leafs") {
    val a = Leaf('a', 1)
    val b = Leaf('b', 2)
    val trees = List(a, b)

    val expectedList = List(Fork(a, b, List('a', 'b'), 3))
    assert(until(Huffman.singleton, Huffman.combine)(trees) === expectedList)
  }

  test("until: combines three leafs") {
    val a = Leaf('a', 1)
    val b = Leaf('b', 2)
    val c = Leaf('c', 4)
    val trees = List(a, b, c)

    val expectedList = List(
      Fork(
        Fork(a, b, List('a', 'b'), 3),
        c,
        List('a', 'b', 'c'),
        7
      )
    )
    val actualList = until(Huffman.singleton, Huffman.combine)(trees)
    assert(actualList === expectedList)
  }

  test("createCodeTree") {
    val l = List('a', 'b', 'a')
    assert(createCodeTree(l) === Fork(Leaf('b', 1), Leaf('a', 2), List('b', 'a'), 3))
  }

  test("decode: empty bits list") {
    val tree = Fork(
      Leaf('a', 1),
      Leaf('b', 1),
      List('a', 'b'),
      2
    )
    val bits = List()
    assert(decode(tree, bits) === List())
  }

  test("decode: one fork, two leafs") {
    val tree = Fork(
      Leaf('a', 1),
      Leaf('b', 1),
      List('a', 'b'),
      2
    )
    val bits = List(0, 1)
    assert(decode(tree, bits) === List('a', 'b'))
  }

  test("decode: multiple forks") {
    val leftTree = Leaf('c', 1)
    val rightTree = Fork(
      Leaf('a', 1),
      Leaf('b', 1),
      List('a', 'b'),
      2
    )
    val tree = Fork(
      leftTree,
      rightTree,
      List('a', 'b', 'c'),
      3
    )
    val bits = List(0, 1, 0, 1, 1)
    assert(decode(tree, bits) === List('c', 'a', 'b'))
  }

  test ("decodedSecret") {
    println(decodedSecret)
  }

  test("encode: single fork with two leafs") {
    val tree = Fork(
      Leaf('a', 1),
      Leaf('b', 1),
      List('a', 'b'),
      2
    )
    val text = List('a', 'b')
    assert(encode(tree)(text) === List(0, 1))
  }

  test("encode: multiple forks") {
    val leftTree = Leaf('c', 1)
    val rightTree = Fork(
      Leaf('a', 1),
      Leaf('b', 1),
      List('a', 'b'),
      2
    )
    val tree = Fork(
      leftTree,
      rightTree,
      List('a', 'b', 'c'),
      3
    )
    val text = List('c', 'a', 'b')
    assert(encode(tree)(text) === List(0, 1, 0, 1, 1))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("codeBits") {
    val table = List(
      ('a', List(0)),
      ('b', List(1))
    )
    assert(codeBits(table)('a') === List(0))
    assert(codeBits(table)('b') === List(1))
  }

  test("convert: single fork") {
    val tree = makeCodeTree(
      Leaf('a', 1),
      Leaf('b', 1)
    )
    val codeTable = convert(tree)
    println(codeTable)
    assert(codeTable.head == ('a', List(0)))
    assert(codeTable.tail.head == ('b', List(1)))
  }

  test("convert: multiple forks") {
    val tree = makeCodeTree(
      Leaf('c', 1),
      makeCodeTree(
        Leaf('a', 1),
        Leaf('b', 1)
      )
    )
    val codeTable = convert(tree)
    assert(codeTable.head == ('c', List(0)))
    assert(codeTable.tail.head == ('a', List(1, 0)))
    assert(codeTable.tail.tail.head == ('b', List(1, 1)))
  }

  test("quickEncode: single fork with two leafs") {
    val tree = Fork(
      Leaf('a', 1),
      Leaf('b', 1),
      List('a', 'b'),
      2
    )
    val text = List('a', 'b')
    assert(quickEncode(tree)(text) === List(0, 1))
  }

  test("quickEncode: multiple forks") {
    val leftTree = Leaf('c', 1)
    val rightTree = Fork(
      Leaf('a', 1),
      Leaf('b', 1),
      List('a', 'b'),
      2
    )
    val tree = Fork(
      leftTree,
      rightTree,
      List('a', 'b', 'c'),
      3
    )
    val text = List('c', 'a', 'b')
    assert(quickEncode(tree)(text) === List(0, 1, 0, 1, 1))
  }

  test("decode and quickEncode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
