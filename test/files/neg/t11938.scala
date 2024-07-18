//> using options -Xlint -Werror

class Test {
  val a: Nil.type = (Vector(): Any) match { case n @ Nil       => n } // error
  val b: Nil.type = (Vector(): Any) match { case n @ (m @ Nil) => n } // error was: CCE
}
