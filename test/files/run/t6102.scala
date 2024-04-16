//> using options -opt:inline:** -Werror
//
// scala/bug#6102 Wrong bytecode in lazyval + no-op finally clause

object Test {

  def main(args: Array[String]): Unit = {
    try {
      @annotation.unused val x = 3
    } finally {
      print("hello")
    }
  }
}
