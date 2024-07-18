//> using options -Xfatal-warnings
//
object Test {
  def ==(p: Phase): Int = 0

  def foo: Unit = {
    ==(new Phase())
  }
}

class Phase
