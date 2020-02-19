import scala.tools.nsc.Settings
import scala.tools.partest.{ Hashless, ReplTest }

object Test extends ReplTest with Hashless {
  override def transformSettings(s: Settings) = {
    s.Yreplclassbased.value  = true
    s.YreplMagicImport.value = true
    s
  }

  def code = """
    |class Meter(val value: Int) extends AnyVal
    |val x = new Meter(1)
    |object T { class Meter(val value: Int) extends AnyVal }
    |val y = new T.Meter(2)
    |object S { object T { class Meter(val value: Int) extends AnyVal } }
    |val z = new S.T.Meter(3)
    |""".stripMargin
}
