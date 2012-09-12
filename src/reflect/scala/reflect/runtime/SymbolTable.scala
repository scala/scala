package scala.reflect
package runtime

/**
 *  This symbol table trait fills in the definitions so that class information is obtained by reflection.
 *  It can be used either from a reflexive universe (class scala.reflect.runtime.JavaUniverse), or else from
 *  a runtime compiler that uses reflection to get a class information (class scala.tools.reflect.ReflectGlobal)
 */
// SI-6240: test thread-safety, make trees synchronized as well
trait SymbolTable extends internal.SymbolTable
                     with JavaMirrors
                     with SymbolLoaders
                     with ReflectedNames
                     with ReflectedScopes
                     with ReflectedSymbols
                     with ReflectedTypes {

  def info(msg: => String) =
    if (settings.verbose.value) println("[reflect-compiler] "+msg)

  def debugInfo(msg: => String) =
    if (settings.debug.value) info(msg)

}
