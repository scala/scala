
import scala.tools.partest.{SessionTest, Welcoming}

// scala/bug#7740
object Test extends SessionTest with Welcoming {
  // normalize the "elided" lines because the frame count depends on test context
  lazy val elided = """(\s+\.{3} )\d+( elided)""".r
  lazy val frame  = """(\s+\Qat .f(<console>:\E)\d+(\))""".r
  override def normalize(line: String) = line match {
    case elided(ellipsis, suffix) => s"$ellipsis???$suffix"
    case frame(prefix, suffix)    => s"${prefix}XX${suffix}"
    case s                        => s
  }
  override def expected = super.expected map normalize
}
