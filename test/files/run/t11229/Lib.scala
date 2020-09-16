// scalac: -Werror
sealed trait A
sealed trait AB extends A
sealed trait AA extends A

object Lib {
  def f(a: A): Boolean = a match {
       case _:AA => true
       case _:AB => false
  }
}
