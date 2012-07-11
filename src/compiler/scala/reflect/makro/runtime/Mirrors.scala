package scala.reflect.makro
package runtime

import scala.tools.nsc.util.ScalaClassLoader

trait Mirrors {
  self: Context =>

  import universe._
  import definitions._

  class ContextMirror extends RootsBase(NoSymbol) {
    val universe: self.universe.type = self.universe
    def rootLoader: LazyType = rootMirror.rootLoader

    val RootPackage = rootMirror.RootPackage
    val RootClass = rootMirror.RootClass
    val EmptyPackage = rootMirror.EmptyPackage
    val EmptyPackageClass = rootMirror.EmptyPackageClass

    // [Eugene++] this still doesn't solve the problem of invoking `c.typeCheck` on the code that refers to packageless symbols
    override protected def mirrorMissingHook(owner: Symbol, name: Name): Symbol = {
      if (owner.isRoot && isJavaClass(name.toString)) EmptyPackageClass.info decl name
      else NoSymbol
    }

    private lazy val libraryClasspathLoader: ClassLoader = {
      val classpath = platform.classPath.asURLs
      ScalaClassLoader.fromURLs(classpath)
    }

    private def isJavaClass(path: String): Boolean =
      try {
        Class.forName(path, true, libraryClasspathLoader)
        true
      } catch {
        case (_: ClassNotFoundException) | (_: NoClassDefFoundError) | (_: IncompatibleClassChangeError) =>
        false
      }

    override def toString = "macro context mirror"
  }
}