object Test {
  //println("java.library.path=" + System.getProperty("java.library.path"))
  val model = System.getProperty("sun.arch.data.model", "32")
  System.loadLibrary("natives-" + model)

  @native
  def sayHello(s: String): String = null

  def main(args: Array[String]) {
    val s = sayHello("Scala is great!")
    println("Invocation returned \"" + s + "\"")
  }
}
