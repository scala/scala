import scala.tools.partest._
import scala.tools.nsc.Settings

object Test extends StoreReporterDirectTest {
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
    val global = newCompiler("-cp", classpath, "-d", testOutput.path)
    compileString(newCompiler("-cp", classpath, "-d", testOutput.path))(app)
    //global.reporter.ERROR.foreach(println)
  }

  def show(): Unit = {
    compileLibrary();
    println(filteredInfos.mkString("\n"))
    storeReporter.infos.clear()
    compileApp();
    // we should get "missing or invalid dependency detected" errors, because we're trying to use an implicit that can't be unpickled
    // but we don't know the number of these errors and their order, so I just ignore them all
    println(filteredInfos.filterNot (_.msg.contains("missing or invalid dependency detected")).mkString("\n"))
  }
}
