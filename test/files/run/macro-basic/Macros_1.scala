object Macros {
  object Shmacros {
    def macro foo(x: Int): Int = x
  }
  def macro bar(x: Int): Int = x
}

class Macros {
  def macro quux(x: Int): Int = x
}