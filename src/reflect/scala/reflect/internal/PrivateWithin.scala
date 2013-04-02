package scala.reflect
package internal

import ClassfileConstants._
import java.lang.{ Class => jClass }
import java.lang.reflect.{ Member => jMember }

trait PrivateWithin {
  self: SymbolTable =>

  def importPrivateWithinFromJavaFlags(sym: Symbol, jflags: Int): Symbol = {
    if ((jflags & (JAVA_ACC_PRIVATE | JAVA_ACC_PROTECTED | JAVA_ACC_PUBLIC)) == 0)
      // See ticket #1687 for an example of when topLevelClass is NoSymbol: it
      // apparently occurs when processing v45.3 bytecode.
      if (sym.enclosingTopLevelClass != NoSymbol)
        sym.privateWithin = sym.enclosingTopLevelClass.owner

    // protected in java means package protected. #3946
    if ((jflags & JAVA_ACC_PROTECTED) != 0)
      if (sym.enclosingTopLevelClass != NoSymbol)
        sym.privateWithin = sym.enclosingTopLevelClass.owner

    sym
  }

  def propagatePackageBoundary(c: jClass[_], syms: Symbol*): Unit =
    propagatePackageBoundary(JavaAccFlags(c), syms: _*)
  def propagatePackageBoundary(m: jMember, syms: Symbol*): Unit =
    propagatePackageBoundary(JavaAccFlags(m), syms: _*)
  def propagatePackageBoundary(jflags: JavaAccFlags, syms: Symbol*) {
    if (jflags.hasPackageAccessBoundary)
      syms foreach setPackageAccessBoundary
  }

  // protected in java means package protected. #3946
  // See ticket #1687 for an example of when the enclosing top level class is NoSymbol;
  // it apparently occurs when processing v45.3 bytecode.
  def setPackageAccessBoundary(sym: Symbol): Symbol = (
    if (sym.enclosingTopLevelClass eq NoSymbol) sym
    else sym setPrivateWithin sym.enclosingTopLevelClass.owner
  )
}
