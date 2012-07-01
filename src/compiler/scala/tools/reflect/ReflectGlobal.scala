package scala.tools
package reflect

import scala.tools.nsc.Global
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.Settings

/** A version of Global that uses reflection to get class
 *  infos, instead of reading class or source files.
 */
class ReflectGlobal(currentSettings: Settings, reporter: Reporter, override val rootClassLoader: ClassLoader)
  extends Global(currentSettings, reporter) with scala.tools.reflect.ReflectSetup with scala.reflect.runtime.SymbolTable {

  override def transformedType(sym: Symbol) =
    erasure.transformInfo(sym,
      uncurry.transformInfo(sym,
        refChecks.transformInfo(sym, sym.info)))

  override def isCompilerUniverse = true

  // Typically `runtimeMirror` creates a new mirror for every new classloader
  // and shares symbols between the created mirrors.
  //
  // However we can't do that for the compiler.
  // The problem is that symbol sharing violates owner chain assumptions that the compiler has.
  //
  // For example, we can easily end up with a situation when:
  //
  //   Predef defined in package scala loaded by the classloader that has scala-library.jar
  //
  // cannot be accessed in:
  //
  //   package scala for the rootMirror of ReflectGlobal that might correspond to a different classloader
  //
  // This happens because, despite the fact that `Predef` is shared between multiple `scala` packages (i.e. multiple scopes)
  // (each mirror has its own set package symbols, because of the peculiarities of symbol loading in scala),
  // that `Predef` symbol only has a single owner, and this messes up visibility, which is calculated based on owners, not scopes.
  override def runtimeMirror(cl: ClassLoader): Mirror = rootMirror
}

