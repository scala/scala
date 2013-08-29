// During development of delayed delambdafy there was a problem where
// GenASM would eliminate a loadmodule for all methods defined within that module
// even if those methods were static. This test would thus fail
// with a verify error under -Ydelambdafy:method

object Test {
  def moduleMethod(x: String) = x

  def map(x: String, f: String => String) = f(x)

  def main(args: Array[String]) {
     println(map("hello", Test.moduleMethod))
  }
}