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

  /* VAR's test sets */
  def vs1(e: Int): Boolean = (e == 1) || (e == 3) || (e == 5) || (e == 7) || (e == 9) || (e == 0) || (e == -1)
  def vs2(e: Int): Boolean = (e == -1) || (e == -3) || (e == -5) || (e == -7) || (e == -9) || (e == 0)
  def vsFilter(e: Int): Boolean = (e == 0) || (e == -9) || (e == -1)
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
      assert(!contains(s1, 3), "Singleton s1: does not contain 3")
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

  test("Intersect") {
    new TestSets {
      val s = FunSets.toString(intersect(vs1, vs2))
      assert(s === "{-1,0}", "vs1 intersect vs2")
      val h = FunSets.toString(intersect(vs1, s3))
      assert(h === "{3}", "vs1 intersect s3")
    }
  }

  test("Filter") {
    val s = FunSets.toString(filter(vs1, vsFilter))
    assert(s === "{-1,0}", "vs1 filtered")
    val h = FunSets.toString(filter(vs2, vsFilter))
    assert(h === "{-9,-1,0}", "vs2 filtered")

  }

  test("Forall") {
    assert(forall(vs1, vsFilter) === false, "vs1")
    assert(forall(vs2, vsFilter) === false, "vs2")
    assert(forall(singletonSet(-9), vsFilter) === true, "singletonSet(-9)")
    assert(forall(union(singletonSet(-9), singletonSet(-1)), vsFilter) === true, "{-9, -1}")
  }

  test("Exists") {
    new TestSets {
      assert(exists(vs1, vsFilter) === true, "vs1")
      assert(exists(vs2, vsFilter) === true, "vs2")
      assert(exists(s1, vsFilter) === false, "s1")
      assert(exists(s2, vsFilter) === false, "s2")
      assert(exists(s3, vsFilter) === false, "s3")
      assert(exists(singletonSet(-1), vsFilter) === true, "{-1}")
    }
  }

  test("Map") {
    new TestSets {
      val a = FunSets.toString(map(vs1, x => x + 5))
      assert(a === "{4,5,6,8,10,12,14}", "vs1. f == x + 5")

      val b = FunSets.toString(map(vs1, x => x * x))
      assert(b === "{0,1,9,25,49,81}", "vs1. f == x * x")

    }
  }
}
