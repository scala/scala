import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
    import scala.reflect.runtime.{universe => ru}
    import scala.reflect.runtime.{currentMirror => cm}
    import scala.reflect.api.{Universe => ApiUniverse}
    class A
    lazy val apiru = ru: ApiUniverse
    apiru.typeTag[A].in(cm)
  """
}