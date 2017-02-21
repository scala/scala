package xsbt

import scala.tools.nsc.Global

/**
 * Utility methods for creating (source|binary) class names for a Symbol.
 */
trait ClassName {
  val global: Global
  import global._

  /**
   * Creates a flat (binary) name for a class symbol `s`.
   */
  protected def flatname(s: Symbol, separator: Char) =
    atPhase(currentRun.flattenPhase.next) { s fullName separator }

  /**
   * Create a (source) name for a class symbol `s`.
   */
  protected def className(s: Symbol): Name = pickledName(s)

  /**
   * Create a String (source) name for a class symbol `s`.
   */
  protected def classNameAsString(s: Symbol): String = pickledNameAsString(s)

  /**
   * Create a (source) name for the class symbol `s` with a prefix determined by the class symbol `in`.
   *
   * If `s` represents a package object `pkg3`, then the returned name will be `pkg1.pkg2.pkg3.package`.
   * If `s` represents a class `Foo` nested in package object `pkg3` then the returned name is `pkg1.pkg2.pk3.Foo`.
   */
  protected def classNameAsSeenIn(in: Symbol, s: Symbol): String = atPhase(currentRun.picklerPhase.next) {
    if (in.isRoot || in.isRootPackage || in == NoSymbol || in.isEffectiveRoot)
      s.simpleName.toString
    else if (in.isPackageObjectOrClass)
      in.owner.fullName + "." + s.name
    else
      in.fullName + "." + s.name
  }

  private def pickledName(s: Symbol): Name =
    atPhase(currentRun.picklerPhase.next) { s.fullNameAsName('.') }

  private def pickledNameAsString(s: Symbol): String =
    atPhase(currentRun.picklerPhase.next) { s.fullName }

  protected def isTopLevelModule(sym: Symbol): Boolean =
    atPhase(currentRun.picklerPhase.next) {
      sym.isModuleClass && !sym.isImplClass && !sym.isNestedClass
    }

  protected def flatclassName(s: Symbol, sep: Char, dollarRequired: Boolean): String =
    flatname(s, sep) + (if (dollarRequired) "$" else "")
}
