import scala.tools.partest._
import scala.tools.nsc.Settings

object Test extends DirectTest {
  override def extraSettings = "-cp " + sys.props("partest.lib") + " -d \"" + testOutput.path + "\""

  def code = """
    object Test extends App {
      // manifest lookup also involves type tag lookup
      // because we support manifest <-> typetag convertability
      //
      // however when scala-reflect.jar (the home of type tags) is not on the classpath
      // we need to omit the type tag lookup, because we lack the necessary symbols
      // to do implicit search and tag materialization
      // (such missing symbols are e.g. ApiUniverseClass and TypeTagsClass)
      //
      // the test case you're looking at checks exactly this
      // we establish a classpath that only includes scala-library.jar
      // and then force scalac to perform implicit search for a manifest
      // if type tag lookup is not disabled, the compiler will crash
      // if it is disabled, then the compilation will succeed
      // http://groups.google.com/group/scala-internals/browse_thread/thread/166ce4b71b7c46bb
      def foo[T: Manifest] = ()
      foo[List[Int]]
    }
  """

  def show = compile()
}