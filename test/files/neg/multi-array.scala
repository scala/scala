/** Multi-dimensional array creation with `new` was removed in 2.10.
 *  The replacement Array.ofDim[Int](10,10) makes the original mistake
 *  which was tested here impossible.
 *  This test will fail now because the constructor doesn't exist anymore.
 */
class Foo {
  val a: Array[Int] = new Array(10, 10)
}

//Before removal of constructor non-unary Array constructors:
/** Check that a multi-dimensional array can't be created
 *  when the wrong number of arguments w.r.t. to the array's
 *  type is given.
 */
