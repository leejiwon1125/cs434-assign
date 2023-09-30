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

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("final test"){
    //set object dictionary
    val dictionary:List[Char] = string2Chars("this is huffman code test for all alphabet and something other. all alphabet: qwertyuiopasdfghjklzxcvbnm")
    //set leaf node
    //val a = times(dictionary)
    //val leaf_list:List[Leaf] = makeOrderedLeafList(a)
    val code_tree:CodeTree = createCodeTree(dictionary)
    val encode_normal = encode(code_tree)(string2Chars("normal decode"))
    val encode_quick = quickEncode(code_tree)(string2Chars("quick decode"))
    val encode_quick2 = quickEncode(code_tree)(string2Chars("normal decode"))

    //check if encode-decode is corret for each normal and quick
    assert(decode(code_tree,encode_normal).equals(string2Chars("normal decode")))
    assert(decode(code_tree,encode_quick).equals(string2Chars("quick decode")))
    //check if two strategis make same result
    assert(encode_normal.equals(encode_quick2))

  }

}
