package scala.reflect
package internal

import ClassfileConstants._

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
}