object Test {

  //println("java.library.path=" + System.getProperty("java.library.path"))
  
  val sysWordSize = System.getProperty("sun.arch.data.model", "32")
  val sysType = System.getProperty("os.name")
  
  val libName =
    if (sysType == "Mac OS X")
      "natives"
    else
      "natives-" + sysWordSize
  
  System.loadLibrary(libName)

  @native
  def sayHello(s: String): String = null

  def main(args: Array[String]) {
    val s = sayHello("Scala is great!")
    println("Invocation returned \"" + s + "\"")
  }
}
