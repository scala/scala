import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.shell.{ILoop, ShellConfig}
import scala.tools.partest.{hexdump, ReplTest}

object Test extends ReplTest {
  def code = s"""
    |java.nio.CharBuffer.allocate(5)
    |java.nio.CharBuffer.allocate(6)
    |class C
    |classOf[C]
    |val esc = 0x1b.toChar
    |classOf[C].toString + esc + "[3z"
    |classOf[C].toString + esc + "[3!"
    |classOf[C].toString + scala.io.AnsiColor.YELLOW
    |"${"\\"}uCAFE caffè"
    |""".stripMargin

  override protected def shellConfig(testSettings: Settings) =
    new ILoop.TestConfig(ShellConfig(testSettings)) {
      override val colorOk = true
    }
  override def normalize(s: String) = hexdump(s).mkString("", "\n", "\n")
}
