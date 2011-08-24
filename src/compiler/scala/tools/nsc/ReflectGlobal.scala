package scala.tools.nsc

import reporters.Reporter

/** A version of Global that uses reflection to get class
 *  infos, instead of reading class or source files.
 */
class ReflectGlobal(currentSettings: Settings, reporter: Reporter)
  extends Global(currentSettings, reporter) with reflect.runtime.SymbolTable {

  override def transformedType(sym: Symbol) =
    erasure.transformInfo(sym,
      uncurry.transformInfo(sym,
        refChecks.transformInfo(sym, sym.info)))

}
