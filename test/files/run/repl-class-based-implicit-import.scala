import scala.tools.nsc.Settings
import scala.tools.partest.ReplTest

// https://issues.apache.org/jira/browse/SPARK-5150
object Test extends ReplTest {

  override def transformSettings(s: Settings) = {
    s.Yreplclassbased.value = true
    s
  }

  def code = """
    |def showInt(implicit x: Int) = println(x)
    |object IntHolder { implicit val myInt = 5 }
    |import IntHolder.myInt
    |showInt
    |class A; showInt
    |class B { showInt }
    |""".stripMargin
}
