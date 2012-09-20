import scala.tools.partest._

object Test extends DirectTest {
  def code = ???

  def library = """
    import scala.reflect.runtime.universe._

    object Library {
      def foo[T: TypeTag] = ()
    }
  """
  def compileLibrary() = {
    val classpath = List(sys.props("partest.lib"), sys.props("partest.reflect")) mkString sys.props("path.separator")
    compileString(newCompiler("-cp", classpath, "-d", testOutput.path))(library)
  }

  def app = """
    object Test extends App {
      // tries to materialize a type tag not having scala-reflect.jar on the classpath
      // even though it's easy to materialize a type tag of Int, this line will fail
      // because materialization involves classes from scala-reflect.jar
      //
      // in this test we make sure that the compiler doesn't crash
      // but just displays several missing class file errors and an unavailable implicit message
      Library.foo[Int]
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
    // we should get bad symbolic reference errors, because we're trying to call a method that can't be unpickled
    // but we don't know the number of these errors and their order, so I just ignore them all
    baos.toString.split("\n") filter (!_.startsWith("error: bad symbolic reference")) foreach println
    System.setErr(prevErr)
  }
}