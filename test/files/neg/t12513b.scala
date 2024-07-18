
//> using options -Werror -Xsource:3

object X { type T = annotation.tailrec }
object Y { type T = annotation.tailrec }
object Z {
  import X.*, Y.*             // OK, both T mean tailrec
  @T def f: Int = { f ; 42 }  // the annotation worked: error, f is not tail recursive
}
