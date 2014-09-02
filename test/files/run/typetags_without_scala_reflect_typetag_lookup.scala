import scala.tools.partest._

object Test extends StoreReporterDirectTest {
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
    compileLibrary();
    println(filteredInfos.mkString("\n"))
    storeReporter.infos.clear()
    compileApp();
    // we should get "missing or invalid dependency detected" errors, because we're trying to use an implicit that can't be unpickled
    // but we don't know the number of these errors and their order, so I just ignore them all
    println(filteredInfos.filterNot(_.msg.contains("missing or invalid dependency detected")).mkString("\n"))
  }
}
