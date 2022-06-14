// scalac: -Werror -Xlint:unused
import scala.language.experimental.macros

object Unused {
  // seen as used before expansion
  private def usedMacro(): Unit = macro UnusedMacro.usedMacroImpl

  // never used
  private def unusedMacro(): Unit = macro UnusedMacro.usedMacroImpl

  def f() = usedMacro()

  private def `fixme macro defs are eliminated at refchecks` = println("Restore macro warnings")
}
