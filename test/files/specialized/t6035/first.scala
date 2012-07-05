


/* Unable to include a test case, since this would require separate compilation,
 * which cannot be simulated in partest.
 *  
 * For documentation purposes, these test files are included.
 */
trait Foo[@specialized(Int) A] {
  def foo(x: A): A
}


abstract class Inter extends Foo[Int]


