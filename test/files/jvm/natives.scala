//> using javaOpt -Dneeds.to.fork

object Test {

  //println("java.library.path=" + System.getProperty("java.library.path"))

  val libName =
    // set to 'natives' for freshly built binary on linux. See mkLibNatives.sh
    sys.env.getOrElse("scala_test_nativelib", {
      val os = sys.props("os.name")
      val arch = sys.props("os.arch")
      (os, arch) match {
        case ("Mac OS X", "aarch64") =>
          "natives-arm"
        case ("Mac OS X", "x86_64") =>
          "natives-x86"
        case _ =>
          val wordSize = sys.props.getOrElse("sun.arch.data.model", "32")
          s"natives-$wordSize"
      }
    })

  System.loadLibrary(libName)

  @native
  def sayHello(s: String): String = null

  def main(args: Array[String]): Unit = {
    val s = sayHello("Scala is great!")
    println("Invocation returned \"" + s + "\"")
  }
}
