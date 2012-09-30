import scala.tools.partest._

object Test extends DirectTest {
  def code = ???

  def library = """
    import scala.reflect.runtime.universe._

    trait Library {
      type T
      implicit val tt: TypeTag[T]
    }
  """
  def compileLibrary() = {
    val classpath = List(sys.props("partest.lib"), sys.props("partest.reflect")) mkString sys.props("path.separator")
    compileString(newCompiler("-cp", classpath, "-d", testOutput.path))(library)
  }

  def app = """
    trait App extends Library {
      // tries to create a manifest from a type tag without having scala-reflect.jar on the classpath
      // even though it's possible to convert a type tag into a manifest, this will fail
      // because conversion requires classes from scala-reflect.jar
      //
      // in this test we make sure that the compiler doesn't crash
      // but just displays several missing class file errors and an unavailable implicit message
      manifest[T]
    }
  """
  def compileApp() = {
    val classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")
    compileString(newCompiler("-cp", classpath, "-d", testOutput.path))(app)
  }

  def show(): Unit = {
    val prevErr = System.err
    val baos = new java.io.ByteArrayOutputStream();
    System.setErr(new java.io.PrintStream(baos));
    compileLibrary();
    compileApp();
    // we should get bad symbolic reference errors, because we're trying to use an implicit that can't be unpickled
    // but we don't know the number of these errors and their order, so I just ignore them all
    baos.toString.split("\n") filter (!_.startsWith("error: bad symbolic reference")) foreach println
    System.setErr(prevErr)
  }
}