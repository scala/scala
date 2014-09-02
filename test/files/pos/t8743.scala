import annotation.varargs

object VarArgs {
   @varargs
   def foo[A](x: A, xs: String*): A = ???

   @varargs
   def bar[A](x: List[A], xs: String*): A = ???

   @varargs
   def baz[A](x: List[A], xs: String*): A = ???

   @varargs
   def boz[A](x: A, xs: String*): Nothing = ???
}
