package scala.tools.nsc

import reporters.Reporter

/** A version of Global that uses reflection to get class
 *  infos, instead of reading class or source files.
 */
class ReflectGlobal(currentSettings: Settings, reporter: Reporter, var classLoader: ClassLoader)
  extends Global(currentSettings, reporter) with reflect.runtime.SymbolTable {

  override def transformedType(sym: Symbol) =
    erasure.transformInfo(sym,
      uncurry.transformInfo(sym,
        refChecks.transformInfo(sym, sym.info)))

  override def staticClass(fullname: String) =
    super[SymbolTable].staticClass(fullname)

  override def staticModule(fullname: String) =
    super[SymbolTable].staticModule(fullname)
}
