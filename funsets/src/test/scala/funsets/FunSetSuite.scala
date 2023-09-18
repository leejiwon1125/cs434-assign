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
  ignore("string take") {
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
  ignore("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  ignore("contains is implemented") {
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
    val s4 = singletonSet(4)
    val s5 = singletonSet(5)
    val s6 = singletonSet(6)
    val s123 = union(union(s1,s2),s3)
    val s456 = union(union(s4,s5),s6)
    val s234 = union(union(s2,s3),s4)
    val u = union(s123,s456)
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

  test("union test"){
    new TestSets {

      assert(contains(s123,1) && contains(s123,2) && contains(s123,3),"union 123")
      assert(!contains(s123,4),"union 123")
      assert(!contains(s123,5),"union 123")
      assert(!contains(s123,6),"union 123")

    }
  }

  test("intersect test") {
    new TestSets {
      val empty_set = intersect(s123,s456)
      val s23 = intersect(s123,s234)

      assert(!contains(empty_set,1), "intersect 123, 456")
      assert(!contains(empty_set,2), "intersect 123, 456")
      assert(!contains(empty_set,3), "intersect 123, 456")
      assert(!contains(empty_set,4), "intersect 123, 456")
      assert(!contains(empty_set,5), "intersect 123, 456")
      assert(!contains(empty_set,6), "intersect 123, 456")

      assert(contains(s23, 2), "intersect 123, 234")
      assert(contains(s23, 3), "intersect 123, 234")
      assert(!contains(s23, 1), "intersect 123, 234")
      assert(!contains(s23, 4), "intersect 123, 234")
      assert(!contains(s23, 5), "intersect 123, 234")
      assert(!contains(s23, 6), "intersect 123, 234")

    }
  }

  test("diff test") {
    new TestSets {

      val s1_ = diff(s123, s234)
      assert(contains(s1_,1),"diff 123, 234")
      assert(!contains(s1_,2),"diff 123, 234")
      assert(!contains(s1_,3),"diff 123, 234")
      assert(!contains(s1_,4),"diff 123, 234")

    }
  }


  test("filter test"){
    new TestSets {

      val s123_ = filter(u, (x=> x<=3))
      assert(contains(s123_, 1) && contains(s123_, 2) && contains(s123_, 3), "filter 123")
      assert(!contains(s123_, 4), "filter 123")
      assert(!contains(s123_, 5), "filter 123")
      assert(!contains(s123_, 6), "filter 123")

    }
  }


  test("forall test") {
    new TestSets {

      assert(forall(s123, (x => x <= 3)), "forall 123")
      assert(forall(s123, (x => x <= 4)), "forall 123")
      assert(!forall(s123, (x => x <= 2)), "forall 123")

    }
  }

  test("exists test") {
    new TestSets {

      assert(exists(s123, (x => x == 3)), "exists 123")
      assert(!exists(s123, (x => x == 4)), "exists 123")

    }
  }

  test("map test") {
    new TestSets {
      val s246 = map(s123, (x => x*2))
      printSet(s246)
      assert(contains(s246, 2), "map 123")
      assert(contains(s246, 4), "map 123")
      assert(contains(s246, 6), "map 123")
      assert(!contains(s246, 1), "map 123")
      assert(!contains(s246, 3), "map 123")
      assert(!contains(s246, 5), "map 123")

    }
  }
  ignore("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
}
