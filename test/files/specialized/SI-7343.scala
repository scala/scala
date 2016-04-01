class Parent[@specialized(Int) T]

object Test extends App {

  /**
   * This method will check if specialization is correctly rewiring parents
   * for classes defined inside methods. The pattern is important since this
   * is how closures are currently represented: as locally-defined anonymous
   * classes, which usually end up inside methods. For these closures we do
   * want their parents rewired correctly:
   *
   * ```
   *  def checkSuperClass$mIc$sp[T](t: T, ...) = {
   *    class X extends Parent$mcI$sp // instead of just Parent
   *    ...
   *  }
   */
  def checkSuperClass[@specialized(Int) T](t: T, expectedXSuper: String) = {
    // test target:
    //  - in checkSuperClass, X should extend Parent
    //  - in checkSuperClass$mIc$sp, X should extend Parent$mcI$sp
    class X extends Parent[T]()

    // get the superclass for X and make sure it's correct
    val actualXSuper = (new X).getClass().getSuperclass().getSimpleName()
    assert(actualXSuper == expectedXSuper, actualXSuper + " != " + expectedXSuper)
  }

  checkSuperClass("x", "Parent")
  checkSuperClass(101, "Parent$mcI$sp")

  /**
   * This is the same check, but in value. It should work exactly the same
   * as its method counterpart.
   */
  class Val[@specialized(Int) T](t: T, expectedXSuper: String) {
    val check: T = {
      class X extends Parent[T]()

      // get the superclass for X and make sure it's correct
      val actualXSuper = (new X).getClass().getSuperclass().getSimpleName()
      assert(actualXSuper == expectedXSuper, actualXSuper + " != " + expectedXSuper)
      t
    }
  }

  new Val("x", "Parent")
  new Val(101, "Parent$mcI$sp")

  /**
   * NOTE: The same check, only modified to affect constructors, won't
   * work since the class X definition will always be lifted to become a
   * member of the class, making it impossible to force its duplication.
   */
}
