
import scala.tools.partest.ReplTest
import scala.tools.nsc.Settings


/*
 * Add // show to witness:
 *     val $line3$read: $line3.$read.INSTANCE.type = $line3.$read.INSTANCE;
 */
object Test extends ReplTest {
  override def transformSettings(settings: Settings): Settings = {
    settings.Yreplclassbased.value = true
    settings
  }
  def code = 
    """
:paste < EOF
case class C(i: Int)
val c = C(42)
EOF
val d: C = c // shew
    """
}
