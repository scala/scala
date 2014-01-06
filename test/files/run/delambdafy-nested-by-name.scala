// during development of delayed delambdafication I created a bug where calling a by-name method with a by-name argument that
// itself contained a by-name argument would cause a class cast exception. That bug wasn't found in the existing test suite
// so this test covers that case
object Test {
  def meth1(arg1: => String) = arg1
  def meth2(arg2: => String) = meth1({println("hello"); arg2})

  def main(args: Array[String]) {
    println(meth2("world"))
  }
}