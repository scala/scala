
import scala.language.experimental.macros

object Unused {
  private def usedMacro(): Unit = macro UnusedMacro.usedMacroImpl

  def f() = usedMacro()
}
