package xsbt

/**
 * Utility methods for creating (source|binary) class names for a Symbol.
 */
trait ClassName {
  val global: CallbackGlobal
  import global._

  /**
   * Creates a flat (binary) name for a class symbol `s`.
   */
  protected def flatname(s: Symbol, separator: Char) =
    atPhase(currentRun.flattenPhase.next) { s fullName separator }

  /**
   * Create a (source) name for a class symbol `s`.
   */
  protected def className(s: Symbol): String = pickledName(s)

  private def pickledName(s: Symbol): String =
    atPhase(currentRun.picklerPhase) { s.fullName }

  protected def isTopLevelModule(sym: Symbol): Boolean =
    atPhase(currentRun.picklerPhase.next) {
      sym.isModuleClass && !sym.isImplClass && !sym.isNestedClass
    }

  protected def flatclassName(s: Symbol, sep: Char, dollarRequired: Boolean): String =
    flatname(s, sep) + (if (dollarRequired) "$" else "")
}
