package patmat

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times method") {
    assert(times(List('a', 'b', 'b')).groupBy(identity) === List(('a', 1), ('b', 2)).groupBy(identity))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("singleton") {
    assert(!singleton(List()))
    assert(singleton(List(Leaf('a', 1))))
    assert(!singleton(List(Leaf('a', 1), Leaf('b', 2))))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("until of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))

    assert(until(singleton, combine)(leaflist) ===
      List(Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), List('e', 't', 'x'), 7)))
    assert(until(singleton, combine)(List(Leaf('a', 1))) === List(Leaf('a', 1)))
  }

  test("createCodeTree") {
    assert(createCodeTree(List('e', 't', 't', 'x', 'x', 'x', 'x')) ===
      Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), List('e', 't', 'x'), 7))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      val codeTree: CodeTree = createCodeTree("abcdefghabcdabcdabcabc".toList)
      assert(decode(codeTree, encode(codeTree)("abcdefghabcdabcdabcabc".toList)) === "abcdefghabcdabcdabcabc".toList)
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
      assert(encode(frenchCode)(decodedSecret) === secret)
    }
  }

}
