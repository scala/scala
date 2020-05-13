import scala.tools.nsc.Settings
import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def transformSettings(s: Settings) = {
    s.Yreplclassbased.value = true
    s
  }

  def code = """
    |sealed abstract class Value; object Value {
    |  final case class Num(value: Double) extends Value
    |  final case class Str(value: String) extends Value
    |  final case class Bool(value: Boolean) extends Value
    |}
    |class C { final case class Num(value: Double) } // here it should still warn
    |""".stripMargin // scala/bug#11902
}

/* was:

scala> sealed abstract class Value; object Value {
  final case class Num(value: Double) extends Value
  final case class Str(value: String) extends Value
  final case class Bool(value: Boolean) extends Value
}
warning: there were three unchecked warnings; for details, enable `:setting -unchecked' or `:replay -unchecked'
defined class Value
defined object Value

scala> class C { final case class Num(value: Double) } // here it should still warn
warning: there was one unchecked warning; for details, enable `:setting -unchecked' or `:replay -unchecked'
defined class C

 */
