object Test {
  def ==(p: Phase): Int = 0

  def foo {
    ==(new Phase())
  }
}

class Phase
