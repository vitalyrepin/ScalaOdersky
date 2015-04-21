package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
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

  test("countChar") {
    assert(countChar('a', List('a', 'a', 'a', 'c', 'd', 'a')) === 4)
  }

  test("removeChar") {
    assert(removeChar('a', List('a', 'b', 'a')) === List('b'))
  }
  test("times") {
    val tms = times(List('a', 'a', 'c', 'e', 'c', 'd', 'a'))
    assert(tms === List(('a', 3), ('c', 2), ('e', 1), ('d', 1)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3), Leaf('a', 4))
    val combine1 = combine(leaflist)
    assert(combine1 === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 3), Leaf('a', 4)))
    val combine2 = combine(combine1)
    assert(combine2.head === Leaf('a', 4)) // Checking that sorting order is kept
  }

  test("decoded secret") {
    assert(decodedSecret === string2Chars("huffmanestcool"))
  }

  def encodeDecodeTests(encoder: CodeTree => List[Char] => List[Bit], prefix: String): Unit = {
    test(prefix + ": encode with test trees") {
      new TestTrees {
        assert(encoder(t1)("a".toList) === List(0))
        assert(encoder(t1)("b".toList) === List(1))
        assert(encoder(t1)("ab".toList) === List(0, 1))
        assert(encoder(t1)("ababba".toList) === List(0, 1, 0, 1, 1, 0))
      }
    }

    test(prefix + ": decode and encode a very short text should be identity") {
      new TestTrees {
        assert(decode(t1, encoder(t1)("abbbaab".toList)) === "abbbaab".toList)
        assert(decode(t2, encoder(t2)("abddba".toList)) === "abddba".toList)
        val str = "astalavistababy"
        assert(decode(frenchCode, encoder(frenchCode)(str.toList)) === str.toList)
      }
    }
  }

  encodeDecodeTests(encode, "Legacy encode")

  encodeDecodeTests(quickEncode, "Quick encode")

  test("quick encode: convert function correctness") {
    new TestTrees {
      val tbl = convert(t2)
      assert(convert(t2) === List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1))))
    }
  }
}
