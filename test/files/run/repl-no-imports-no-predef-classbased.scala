object Test extends scala.tools.partest.ReplTest {

  override def transformSettings(settings: scala.tools.nsc.Settings) = {
    settings.noimports.value = true
    settings.nopredef.value = true
    settings.Yreplclassbased.value = true
    settings
  }

  def code = """
case class K(s: java.lang.String)
class C { implicit val k: K = K("OK?"); override def toString = "C(" + k.toString + ")" }
val c = new C
import c.k
scala.Predef.implicitly[K]
val k = 42
k                 // was K(OK?)
"""
}
