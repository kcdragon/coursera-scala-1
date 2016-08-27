package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite {

  test("wordOccurrences: single letter word") {
    assert(wordOccurrences("a") === List(('a', 1)))
  }

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }

  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }

  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }

  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }

  test("subtract: a - nil") {
    val a = List(('a', 1))
    assert(subtract(a, Nil) === a)
  }

  test("subtract: a - a") {
    val a = List(('a', 1))
    assert(subtract(a, a) === Nil)
  }

  test("subtract: aa - a") {
    val a = List(('a', 1))
    val aa = List(('a', 2))
    assert(subtract(aa, a) === a)
  }

  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }

  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: aa") {
    val aa = List(('a', 2))
    val aacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2))
    )
    assert(combinations(aa).toSet === aacomb.toSet)
  }

  test("combinations: ab") {
    val ab = List(('a', 1), ('b', 1))
    val abcomb = List(
      List(),
      List(('a', 1)),
      List(('b', 1)),
      List(('a', 1), ('b', 1))
    )
    assert(combinations(ab).toSet === abcomb.toSet)
  }

  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    assert(combinations(abba).toSet === abbacomb.toSet)
  }

  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }

  // test("sentence anagrams: single word dictionary") {
  //   val dictionary = List(
  //     "a"
  //   )
  //   Anagrams.dictionary = dictionary
  //   val sentence = List(
  //     "a"
  //   )
  //   val anagrams = List(
  //     List("a")
  //   )
  //   assert(sentenceAnagrams(sentence).toSet === anagrams.toSet)
  // }

  // test("sentence anagrams: does not include words not in dictionary") {
  //   val dictionary = List(
  //     "a",
  //     "ab"
  //   )
  //   Anagrams.dictionary = dictionary
  //   val sentence = List(
  //     "ab"
  //   )
  //   val anagrams = List(
  //     List("ab")
  //   )
  //   assert(sentenceAnagrams(sentence).toSet === anagrams.toSet)
  // }

  // test("sentence anagrams: two words, one char") {
  //   val dictionary = List(
  //     "a",
  //     "aa"
  //   )
  //   Anagrams.dictionary = dictionary
  //   val sentence1 = List(
  //     "aa"
  //   )
  //   val sentence2 = List(
  //     "a", "a"
  //   )
  //   val anagrams = List(
  //     List("a", "a"),
  //     List("aa")
  //   )
  //   assert(sentenceAnagrams(sentence1).toSet === anagrams.toSet)
  //   assert(sentenceAnagrams(sentence2).toSet === anagrams.toSet)
  // }

  // test("sentence anagrams: three words, two chars") {
  //   val dictionary = List(
  //     "a",
  //     "b",
  //     "ab"
  //   )
  //   Anagrams.dictionary = dictionary
  //   val sentence1 = List(
  //     "ab"
  //   )
  //   val anagrams = List(
  //     List("a", "b"),
  //     List("b", "a"),
  //     List("ab")
  //   )
  //   assert(sentenceAnagrams(sentence1).toSet === anagrams.toSet)
  // }

  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }

}
