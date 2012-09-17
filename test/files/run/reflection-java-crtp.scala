import scala.tools.partest._
import scala.tools.nsc.Settings

object Test extends ReplTest {
  def code = """
    import scala.reflect.runtime.universe._
    val enum = typeOf[SimpleEnumeration].baseClasses(1).asClass
    // make sure that the E's in Enum<E extends Enum<E>> are represented by the same symbol
    val e1 = enum.typeParams(0).asType
    val TypeBounds(_, TypeRef(_, _, List(TypeRef(_, e2: TypeSymbol, _)))) = e1.typeSignature
    println(e1 eq e2)
  """

  override def transformSettings(settings: Settings): Settings = {
    val thisFile = testPath.jfile.getAbsolutePath
    val javaCompiledAnnotationsJar = (thisFile stripSuffix "scala") + "jar"
    val classpath = List(sys.props("partest.lib"), sys.props("partest.reflect"), sys.props("partest.comp"), javaCompiledAnnotationsJar) mkString sys.props("path.separator")
    settings.processArguments(List("-cp", classpath), true)
    settings
  }
}