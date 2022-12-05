package funsets

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite extends munit.FunSuite:

  import FunSets.*

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

  trait TestSets:
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * .ignore annotation.
   */
  test("singleton set one contains one") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets:
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
  }

  test("union contains all elements of each set") {
    new TestSets:
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
  }

  test("intersect contains all elements contained by both sets") {
    new TestSets:
      val s = intersect(union(s1, s3), union(s1, s2))
      assert(contains(s, 1), "intersect contains 1")
      assert(!contains(s, 2), "intersect does not contain 2")
      assert(!contains(s, 3), "intersect does not contain 3")
  }

  test("diff contains all elements contained by diffed set but not contained by diffing set") {
    new TestSets:
      val s = diff(union(s1, s3), union(s1, s2))
      assert(!contains(s, 1), "diff does not contain 1")
      assert(!contains(s, 2), "diff does not contain 2")
      assert(contains(s, 3), "diff contains 3")
  }

  test("filter contains all elements contained by filtered set and holds filter condition") {
    new TestSets:
      val s = filter(union(union(s1, s3), union(s1, s2)), (e: Int) => e == 2)
      assert(!contains(s, 1), "filter does not contain 1")
      assert(contains(s, 2), "filter contains 2")
      assert(!contains(s, 3), "filter does not contain 3")
  }

  test("forall all elements contained by set should hold predict condition") {
    new TestSets:
      assert(forall(union(union(s1, s3), union(s1, s2)), (e: Int) => e < 4), "forall elements less than 4")
  }

  test("exists at least one elements contained by set should hold predict condition") {
    new TestSets:
      assert(exists(union(union(s1, s3), union(s1, s2)), (e: Int) => e < 2), "exists elements less than 4")
  }

  test("map elements contained by set should be contained by mapped set after mapped") {
    new TestSets:
      val s = union(union(s1, s3), union(s1, s2))
      val f = (e: Int) => e * 2
      assert(contains(map(s, f), 2), "mapped set contains 2")
      assert(contains(map(s, f), 4), "mapped set contains 4")
      assert(contains(map(s, f), 6), "mapped set contains 6")
  }


  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
