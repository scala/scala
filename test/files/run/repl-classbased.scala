
import scala.tools.partest.ReplTest
import scala.tools.nsc.Settings

//SI-9740
object Test extends ReplTest {
  override def transformSettings(s: Settings): Settings = {
    s.Yreplclassbased.value = true
    s
  }

  def code = 
    """
case class K(s: String)
class C { implicit val k: K = K("OK?"); override def toString = s"C($k)" }
val c = new C
import c.k
implicitly[K]
val k = 42
k                 // was K(OK?)
    """
}
