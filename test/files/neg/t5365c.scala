//> using options -Xfatal-warnings -Xlint:strict-unsealed-patmat
object C {
  trait Z
  final case class Q(i: Int) extends Z

  def unsealedTrait(z: Z) = z match {
    case Q(_) =>
  }

  def unsealedTraitWithCatchall(z: Z) = z match {
    case Q(_) =>
    case _ =>
  }

  def uncheckedUnsealedTrait(z: Z) = (z: @unchecked) match {
    case Q(_) =>
  }

  def catchBlock() = {
    try { 42 } catch { case MyException(_) => 43 } // Throwable isn't sealed, but don't warn here
  }

  def catchBlockWithTypePattern() = {
    try { 42 } catch { case _: MyException => 43 } // See?  Just behave like Java.
  }

  def partialFunction(): PartialFunction[Throwable, Int] = { // Or like PartialFunction behaves.
    case MyException(_) => 67
  }
}

case class MyException(x: String) extends Exception
