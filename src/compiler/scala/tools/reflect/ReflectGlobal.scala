package scala.tools
package reflect

import scala.reflect.internal.util.ScalaClassLoader
import scala.tools.nsc.Global
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.Settings
import scala.tools.nsc.typechecker.Analyzer

/** A version of Global that uses reflection to get class
 *  infos, instead of reading class or source files.
 */
class ReflectGlobal(currentSettings: Settings, reporter: Reporter, override val rootClassLoader: ClassLoader)
  extends Global(currentSettings, reporter) with scala.tools.reflect.ReflectSetup with scala.reflect.runtime.SymbolTable {

  override lazy val analyzer = new {
    val global: ReflectGlobal.this.type = ReflectGlobal.this
  } with Analyzer {
    /** Obtains the classLoader used for runtime macro expansion.
     *
     *  Macro expansion can use everything available in [[global.classPath]] or [[rootClassLoader]].
     *  The [[rootClassLoader]] is used to obtain runtime defined macros.
     */
    override protected def findMacroClassLoader(): ClassLoader = {
      val classpath = global.classPath.asURLs
      ScalaClassLoader.fromURLs(classpath, rootClassLoader)
    }
  }

  override def transformedType(sym: Symbol) =
    postErasure.transformInfo(sym,
      erasure.transformInfo(sym,
        uncurry.transformInfo(sym, sym.info)))

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

  // Mirror and RuntimeClass come from both Global and reflect.runtime.SymbolTable
  // so here the compiler needs an extra push to help decide between those (in favor of the latter)
  import scala.reflect.ClassTag
  override type Mirror = JavaMirror
  override implicit val MirrorTag: ClassTag[Mirror] = ClassTag[Mirror](classOf[Mirror])
  override type RuntimeClass = java.lang.Class[_]
  override implicit val RuntimeClassTag: ClassTag[RuntimeClass] = ClassTag[RuntimeClass](classOf[RuntimeClass])
}

