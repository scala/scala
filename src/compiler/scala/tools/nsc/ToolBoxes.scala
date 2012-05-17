package scala.tools.nsc

import util.ScalaClassLoader

trait ToolBoxes { self: Global =>

  def mkToolBox(frontEnd: FrontEnd = mkSilentFrontEnd(), options: String = "") = new ToolBox(frontEnd, options)

  class ToolBox(val frontEnd: FrontEnd, val options: String) extends AbsToolBox {
    def typeCheck(tree0: Tree, pt: Type = WildcardType, freeTypes: Map[FreeType, Type] = Map[FreeType, Type](), silent: Boolean = false, withImplicitViewsDisabled: Boolean = false, withMacrosDisabled: Boolean = false): Tree = {
      val tree = substituteFreeTypes(tree0, freeTypes)
      val currentTyper = typer
      val wrapper1 = if (!withImplicitViewsDisabled) (currentTyper.context.withImplicitsEnabled[Tree] _) else (currentTyper.context.withImplicitsDisabled[Tree] _)
      val wrapper2 = if (!withMacrosDisabled) (currentTyper.context.withMacrosEnabled[Tree] _) else (currentTyper.context.withMacrosDisabled[Tree] _)
      def wrapper (tree: => Tree) = wrapper1(wrapper2(tree))
      wrapper(currentTyper.silent(_.typed(tree, analyzer.EXPRmode, pt)) match {
        case analyzer.SilentResultValue(result) =>
          result
        case error @ analyzer.SilentTypeError(_) =>
          if (!silent) throw new ToolBoxError(this, "reflective typecheck has failed: %s".format(error.err.errMsg))
          EmptyTree
      })
    }

    def inferImplicitValue(pt: Type, silent: Boolean = true, withMacrosDisabled: Boolean = false): Tree =
      // todo. implement this
      ???

    def inferImplicitView(tree: Tree, from: Type, to: Type, silent: Boolean = true, withMacrosDisabled: Boolean = false, reportAmbiguous: Boolean = true): Tree =
      // todo. implement this
      ???

    def resetAllAttrs(tree: Tree): Tree =
      self.resetAllAttrs(tree)

    def resetLocalAttrs(tree: Tree): Tree =
      self.resetLocalAttrs(tree)

    def runExpr(tree0: Tree, freeTypes: Map[FreeType, Type] = Map[FreeType, Type]()): Any = {
      var tree = substituteFreeTypes(tree0, freeTypes)
      // need to reset the tree, otherwise toolbox will refuse to work with it
      // upd. this has to be done by the user himself, otherwise we run into troubles. see SI-5713
//      tree = resetAllAttrs(tree0.duplicate)
      val imported = importer.importTree(tree)
      val toolBox = libraryClasspathMirror.mkToolBox(frontEnd.asInstanceOf[libraryClasspathMirror.FrontEnd], options)
      try toolBox.runExpr(imported)
      catch {
        case ex: toolBox.ToolBoxError =>
          throw new ToolBoxError(this, ex.message, ex.cause)
      }
    }

    // [Eugene] how do I make this work without casts?
    // private lazy val importer = libraryClasspathMirror.mkImporter(self)
    private lazy val importer = libraryClasspathMirror.mkImporter(self).asInstanceOf[libraryClasspathMirror.Importer { val from: self.type }]

    private lazy val libraryClasspathMirror = {
      if (self.forMSIL)
        throw new UnsupportedOperationException("Scala reflection not available on this platform")

      val libraryClassLoader = {
        val classpath = self.classPath.asURLs
        var loader: ClassLoader = ScalaClassLoader.fromURLs(classpath, self.getClass.getClassLoader)

        // [Eugene] a heuristic to detect REPL
        if (self.settings.exposeEmptyPackage.value) {
          import scala.tools.nsc.interpreter._
          val virtualDirectory = self.settings.outputDirs.getSingleOutput.get
          loader = new AbstractFileClassLoader(virtualDirectory, loader) {}
        }

        loader
      }

      new scala.reflect.runtime.Mirror(libraryClassLoader)
    }

    class ToolBoxError(val toolBox: ToolBox, val message: String, val cause: Throwable = null) extends Throwable(message, cause)

    object ToolBoxError extends ToolBoxErrorExtractor {
      def unapply(error: ToolBoxError): Option[(ToolBox, String)] = Some((error.toolBox, error.message))
    }
  }
}