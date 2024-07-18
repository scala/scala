
//> using options -Xlint -Werror

package example

@deprecated("that was then", "0.1")
class then                            // suppressed

object X {
  @deprecated("this is now", "0.5")
  val t = new then                    // suppressed
  val u = new `then`                  // backticked but deprecated ref
}

object Y {
  val y = new then                    // keyword and deprecated ref
}
object Z {
  object then                         // keyword
}
