import scala.tools.partest.ReplTest
import scala.tools.nsc.Settings

object Test extends ReplTest {

  override def transformSettings(s: Settings): Settings = {
    s.Yreplclassbased.value = true
    s
  }

  def code = """:paste < EOF
    |object X {
    |  import scala.language.implicitConversions
    |  implicit def bar(a: Array[String]) = new Foo(a)
    |  class Foo(a: Array[String]) {
    |    def toX: Seq[String] = a.toSeq
    |  }
    |}
    |EOF
    |import X._
    |:paste < EOF
    |class Bar(s: String) {
    |  val x = Array(s).toX
    |}
    |EOF
    | """.stripMargin
}
