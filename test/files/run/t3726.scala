// Blocks which contain application of a function-typed variable are seen as pure by the compiler
//
import annotation.unused

object Test extends App {
  def test(f: () => Int) = {
    @unused val x = f()
    5
  }

  println(test(() => { println("hi there"); 0 }))
}
