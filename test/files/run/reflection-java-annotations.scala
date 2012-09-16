import scala.tools.partest._
import scala.tools.nsc.Settings

object Test extends ReplTest {
  def code = """
    import scala.reflect.runtime.universe._
    val sym = typeOf[Foo].typeSymbol
    sym.typeSignature
    sym.getAnnotations foreach (_.javaArgs)
    println(sym.getAnnotations)
  """

  override def transformSettings(settings: Settings): Settings = {
    val thisFile = testPath.jfile.getAbsolutePath
    val javaCompiledAnnotationsJar = (thisFile stripSuffix "scala") + "jar"
    val classpath = List(sys.props("partest.lib"), sys.props("partest.reflect"), sys.props("partest.comp"), javaCompiledAnnotationsJar) mkString sys.props("path.separator")
    settings.processArguments(List("-cp", classpath), true)
    settings
  }
}