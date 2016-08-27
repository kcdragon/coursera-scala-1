package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains common elements") {
    new TestSets {
      val s = intersect(s1, s2)
      assert(!contains(s, 1), "s1 intersect s2 does not contain 1")
      assert(!contains(s, 2), "s1 intersect s2 does not contain 2")
      assert(!contains(s, 3), "s1 intersect s2 does not contain 3")

      val t = intersect(s1, s1)
      assert(contains(t, 1), "s1 intersect s1 does contain 1")
    }
  }

  test("intersect contains elements in s that are not in t") {
    new TestSets {
      val s = diff(s1, s2)
      assert(contains(s, 1), "s1 diff s2 does contain 1")
      assert(!contains(s, 2), "s1 diff s2 does not contain 2")

      val t = diff(union(s1, s2), s1)
      assert(contains(t, 2), "(s1 union s2) diff s1 does contain 2")
      assert(!contains(t, 1), "(s1 union s2) diff s1 does contain 1")
    }
  }

  test("filter") {
    new TestSets {
      val nullFilter = (x: Int) => true
      val s = filter(s1, nullFilter)
      assert(contains(s, 1), "nullFilter")

      val all = union(union(s1, s2), s3)
      val t = filter(all, (x: Int) => x <= 2)
      assert(contains(t, 1), "x <= 2 contains 1")
      assert(contains(t, 2), "x <= 2 contains 2")
      assert(!contains(t, 3), "x <= 2 does not contain 3")
    }
  }

  test("forall") {
    new TestSets {
      val s = s1
      assert(forall(s1, (x: Int) => true), "when p is always true, forall is always true")

      val t = union(s1, s2)
      assert(forall(t, (x: Int) => x > 0), "when p is true for all elements of t, forall is true")
      assert(!forall(t, (x: Int) => x < 2),  "when p is not true for an element of t, forall is false")
    }
  }

  test("exists") {
    new TestSets {
      val s = s1
      assert(exists(s1, (x: Int) => true), "when p is always true, exists is always true")

      val t = union(s1, s2)
      assert(exists(t, (x: Int) => x > 0), "when p is true for all elements of t, exists is true")
      assert(exists(t, (x: Int) => x < 2),  "when p is true for an element of t, exists is true")
      assert(!exists(t, (x: Int) => x > 2),  "when p is false for all elements of t, exists is false")
    }
  }

  test("maps") {
    new TestSets {
      val s = s1
      val identity = (x: Int) => x
      val mappedSet = map(s, identity)
      assert(mappedSet(1), "when f is identity function s is returned")

      val t = union(s1, s2)
      val timesTwo = (x: Int) => x * 2
      val mappedSet2 = map(t, timesTwo)
      assert(!mappedSet2(1), "1,2 * 2 does not contain 1")
      assert(mappedSet2(2), "1,2 * 2 does contain 2")
      assert(!mappedSet2(3), "1,2 * 2 does not contain 3")
      assert(mappedSet2(4), "1,2 * 2 does contain 4")
    }
  }
}
