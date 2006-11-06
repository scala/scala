/** I know what I am doing is wrong -- since I am about to look into
 *  this bug, I add a test in pending/pos... however, I am afraid that
 *  once this bug is fixed, this test case might go into test/pos
 *  there it adds to the huge number of tiny little test cases.
 *
 * Ideally, an option in the bugtracking system would automatically
 * handle "pos" bugs.
 */
object Test extends Application {

  object Twice {
    def apply(x: int) = x * 2
    def unapply(x: int): Option[Tuple1[int]] =
      if (x % 2 == 0) Some(Tuple1(x / 2))
      else None
  }

  def test(x: int) = x match {
    case Twice(y) => "x is two times "+y
    case _ => "x is odd"
  }

  Console.println(test(3))
  Console.println(test(4))

}
