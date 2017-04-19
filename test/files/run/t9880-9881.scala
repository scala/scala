import scala.tools.partest.ReplTest
import scala.tools.nsc.Settings

object Test extends ReplTest {

  override def transformSettings(s: Settings): Settings = {
    s.Yreplclassbased.value = true
    s
  }

  lazy val normalizeRegex = """(import\s.*)\(.*\)""".r

  override def normalize(s: String): String = normalizeRegex.replaceFirstIn(s, "$1(...)")

  def code =
    """
      |// import in various ways
      |import java.util.Date
      |import scala.util._
      |import scala.reflect.runtime.{universe => ru}
      |import ru.TypeTag
      |
      |// show the imports
      |:imports
      |
      |// should be able to define this class with the imports above
      |class C[T](date: Date, rand: Random, typeTag: TypeTag[T])
    """.stripMargin
}
