
import scala.reflect.macros.blackbox

object UnusedMacro {
  def usedMacroImpl(c: blackbox.Context)(): c.Tree = {
    import c.universe._
    q"""println("apparently unused macro")"""
  }
}
