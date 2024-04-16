//> using options -Werror -Wvalue-discard
object UnitOfTrust {
  import scala.util._

  private def unitRight[A]: Either[A, Unit] = Right(())

  // fails with:
  //   discarded non-Unit value
  def test1: Either[Int, Unit] = Right(Right(()))
  def test2: Either[Int, Unit] = Right(()).map(_ => unitRight[Int])
  def test3: Either[Int, Unit] = Right(()).map { case _ => unitRight[Int] }

  // fails with:
  //   error: type mismatch;
  //   found   : scala.util.Right[Nothing,Unit]
  //   required: Unit => Unit
  def test4: Either[Int, Unit] = Right(()).map(Right(()))

  // was: compiles just fine
  def test5: Either[Int, Unit] = Right(()).map { case _ => unitRight }
  def test6: Either[Int, Unit] = Right(()).map { _ => unitRight }
  def test7: Either[Int, Unit] = Right(()).map(_ => unitRight)
}
