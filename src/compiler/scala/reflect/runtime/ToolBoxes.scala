package scala.reflect
package runtime

import scala.tools.nsc.reporters._
import scala.tools.nsc.ReflectGlobal
import scala.tools.nsc.CompilerCommand
import scala.tools.nsc.Global
import scala.tools.nsc.typechecker.Modes
import scala.tools.nsc.io.VirtualDirectory
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import scala.tools.nsc.util.FreshNameCreator
import scala.reflect.internal.Flags
import scala.tools.nsc.util.{NoSourceFile, NoFile}
import java.lang.{Class => jClass}
import scala.compat.Platform.EOL

trait ToolBoxes extends { self: Universe =>

  def mkToolBox(frontEnd: FrontEnd = mkSilentFrontEnd(), options: String = "") = new ToolBox(frontEnd, options)

  class ToolBox(val frontEnd: FrontEnd, val options: String) extends AbsToolBox {

    class ToolBoxGlobal(settings: scala.tools.nsc.Settings, reporter: Reporter)
    extends ReflectGlobal(settings, reporter, ToolBox.this.classLoader) {
      import definitions._

      private val trace = scala.tools.nsc.util.trace when settings.debug.value

      private final val wrapperMethodName = "wrapper"

      private var wrapCount = 0

      private def nextWrapperModuleName() = {
        wrapCount += 1
        newTermName("__wrapper$" + wrapCount)
      }

      def verifyExpr(expr: Tree): Unit = {
        // Previously toolboxes used to typecheck their inputs before compiling.
        // Actually, the initial demo by Martin first typechecked the reified tree,
        // then ran it, which typechecked it again, and only then launched the
        // reflective compiler.
        //
        // However, as observed in https://issues.scala-lang.org/browse/SI-5464
        // current implementation typechecking is not always idempotent.
        // That's why we cannot allow inputs of toolboxes to be typechecked,
        // at least not until the aforementioned issue is closed.
        val typed = expr filter (t => t.tpe != null && t.tpe != NoType && !t.isInstanceOf[TypeTree])
        if (!typed.isEmpty) throw new ToolBoxError(ToolBox.this, "reflective toolbox has failed: cannot operate on trees that are already typed")

        val freeTypes = this.freeTypes(expr)
        if (freeTypes.length > 0) {
          var msg = "reflective toolbox has failed:" + EOL
          msg += "unresolved free type variables (namely: " + (freeTypes map (ft => "%s %s".format(ft.name, ft.origin)) mkString ", ") + "). "
          msg += "have you forgot to use TypeTag annotations for type parameters external to a reifee? "
          msg += "if you have troubles tracking free type variables, consider using -Xlog-free-types"
          throw new ToolBoxError(ToolBox.this, msg)
        }
      }

      def typeCheckExpr(expr0: Tree, pt: Type, silent: Boolean = false, withImplicitViewsDisabled: Boolean = false, withMacrosDisabled: Boolean = false): Tree = {
        verifyExpr(expr0)

        // need to wrap the expr, because otherwise you won't be able to typecheck macros against something that contains free vars
        // [Eugene] get rid of the copy/paste w.r.t compileExpr
        val freeTerms = this.freeTerms(expr0)
        val freeTermNames = collection.mutable.Map[Symbol, TermName]()
        freeTerms foreach (ft => {
          var name = ft.name.toString
          val namesakes = freeTerms takeWhile (_ != ft) filter (ft2 => ft != ft2 && ft.name == ft2.name)
          if (namesakes.length > 0) name += ("$" + (namesakes.length + 1))
          freeTermNames += (ft -> newTermName(name + nme.MIRROR_FREE_VALUE_SUFFIX))
        })
        var expr = new Transformer {
          override def transform(tree: Tree): Tree =
            if (tree.hasSymbol && tree.symbol.isFreeTerm) {
              tree match {
                case Ident(_) =>
                  Ident(freeTermNames(tree.symbol))
                case _ =>
                  throw new Error("internal error: %s (%s, %s) is not supported".format(tree, tree.productPrefix, tree.getClass))
              }
            } else {
              super.transform(tree)
            }
        }.transform(expr0)
        val dummies = freeTerms map (freeTerm => ValDef(NoMods, freeTermNames(freeTerm), TypeTree(freeTerm.info), Select(Ident(PredefModule), newTermName("$qmark$qmark$qmark"))))
        expr = Block(dummies, expr)

        // [Eugene] how can we implement that?
        // !!! Why is this is in the empty package? If it's only to make
        // it inaccessible then please put it somewhere designed for that
        // rather than polluting the empty package with synthetics.
        val ownerClass    = EmptyPackageClass.newClassWithInfo(newTypeName("<expression-owner>"), List(ObjectClass.tpe), newScope)
        val owner         = ownerClass.newLocalDummy(expr.pos)
        var currentTyper  = typer.atOwner(expr, owner)
        val wrapper1      = if (!withImplicitViewsDisabled) (currentTyper.context.withImplicitsEnabled[Tree] _) else (currentTyper.context.withImplicitsDisabled[Tree] _)
        val wrapper2      = if (!withMacrosDisabled) (currentTyper.context.withMacrosEnabled[Tree] _) else (currentTyper.context.withMacrosDisabled[Tree] _)
        def wrapper       (tree: => Tree) = wrapper1(wrapper2(tree))

        phase = (new Run).typerPhase // need to set a phase to something <= typerPhase, otherwise implicits in typedSelect will be disabled
        currentTyper.context.setReportErrors() // need to manually set context mode, otherwise typer.silent will throw exceptions
        reporter.reset()

        trace("typing (implicit views = %s, macros = %s): ".format(!withImplicitViewsDisabled, !withMacrosDisabled))(showAttributed(expr, true, true, settings.Yshowsymkinds.value))
        wrapper(currentTyper.silent(_.typed(expr, analyzer.EXPRmode, pt)) match {
          case analyzer.SilentResultValue(result) =>
            trace("success: ")(showAttributed(result, true, true, settings.Yshowsymkinds.value))
            var Block(dummies, unwrapped) = result
            var reversedFreeTermNames = freeTermNames map (_.swap)
            // todo. also fixup singleton types
            unwrapped = new Transformer {
              override def transform(tree: Tree): Tree =
                tree match {
                  case Ident(name) if reversedFreeTermNames contains name =>
                    Ident(reversedFreeTermNames(name)) setType tree.tpe
                  case _ =>
                    super.transform(tree)
                }
            }.transform(unwrapped)
            new TreeTypeSubstituter(dummies map (_.symbol), dummies map (dummy => SingleType(NoPrefix, reversedFreeTermNames(dummy.symbol.name)))).traverse(unwrapped)
            unwrapped
          case error @ analyzer.SilentTypeError(_) =>
            trace("failed: ")(error.err.errMsg)
            if (!silent) throw new ToolBoxError(ToolBox.this, "reflective typecheck has failed: %s".format(error.err.errMsg))
            EmptyTree
        })
      }

      def compileExpr(expr: Tree): (Object, java.lang.reflect.Method) = {
        verifyExpr(expr)

        def wrapExpr(expr0: Tree): Tree = {
          def defOwner(tree: Tree): Symbol = tree find (_.isDef) map (_.symbol) match {
            case Some(sym) if sym != null && sym != NoSymbol => sym.owner
            case _ => NoSymbol
          }

          val freeTerms = this.freeTerms(expr0)
          val freeTermNames = collection.mutable.Map[Symbol, TermName]()
          freeTerms foreach (ft => {
            var name = ft.name.toString
            val namesakes = freeTerms takeWhile (_ != ft) filter (ft2 => ft != ft2 && ft.name == ft2.name)
            if (namesakes.length > 0) name += ("$" + (namesakes.length + 1))
            freeTermNames += (ft -> newTermName(name + nme.MIRROR_FREE_VALUE_SUFFIX))
          })
          val expr = new Transformer {
            override def transform(tree: Tree): Tree =
              if (tree.hasSymbol && tree.symbol.isFreeTerm) {
                tree match {
                  case Ident(_) =>
                    Apply(Ident(freeTermNames(tree.symbol)), List())
                  case _ =>
                    throw new Error("internal error: %s (%s, %s) is not supported".format(tree, tree.productPrefix, tree.getClass))
                }
              } else {
                super.transform(tree)
              }
          }.transform(expr0)

          val obj = EmptyPackageClass.newModule(nextWrapperModuleName())
          val minfo = ClassInfoType(List(ObjectClass.tpe), newScope, obj.moduleClass)
          obj.moduleClass setInfo minfo
          obj setInfo obj.moduleClass.tpe
          val meth = obj.moduleClass.newMethod(newTermName(wrapperMethodName))
          def makeParam(fv: Symbol) = {
            // [Eugene] conventional way of doing this?
            val underlying = fv.tpe.resultType
            val tpe = appliedType(definitions.FunctionClass(0).tpe, List(underlying))
            meth.newValueParameter(freeTermNames(fv)) setInfo tpe
          }
          meth setInfo MethodType(freeTerms map makeParam, AnyClass.tpe)
          minfo.decls enter meth
          trace("wrapping ")(defOwner(expr) -> meth)
          val methdef = DefDef(meth, expr changeOwner (defOwner(expr) -> meth))
          val moduledef = ModuleDef(
              obj,
              Template(
                  List(TypeTree(ObjectClass.tpe)),
                  emptyValDef,
                  NoMods,
                  List(),
                  List(List()),
                  List(methdef),
                  NoPosition))
          trace("wrapped: ")(showAttributed(moduledef, true, true, settings.Yshowsymkinds.value))
          var cleanedUp = resetLocalAttrs(moduledef)
          trace("cleaned up: ")(showAttributed(cleanedUp, true, true, settings.Yshowsymkinds.value))
          cleanedUp
        }

        val mdef = wrapExpr(expr)
        val pdef = PackageDef(Ident(nme.EMPTY_PACKAGE_NAME), List(mdef))
        val unit = new CompilationUnit(NoSourceFile)
        unit.body = pdef

        val run = new Run
        reporter.reset()
        run.compileUnits(List(unit), run.namerPhase)
        throwIfErrors()

        val className = mdef.symbol.fullName
        if (settings.debug.value) println("generated: "+className)
        def moduleFileName(className: String) = className + "$"
        val jclazz = jClass.forName(moduleFileName(className), true, classLoader)
        val jmeth = jclazz.getDeclaredMethods.find(_.getName == wrapperMethodName).get
        val jfield = jclazz.getDeclaredFields.find(_.getName == NameTransformer.MODULE_INSTANCE_NAME).get
        val singleton = jfield.get(null)
        (singleton, jmeth)
      }

      def runExpr(expr: Tree, freeTypes: Map[TypeName, Type] = Map[TypeName, Type]()): Any = {
        val freeTerms = this.freeTerms(expr) // need to calculate them here, because later on they will be erased
        val thunks = freeTerms map (fte => () => fte.value) // need to be lazy in order not to distort evaluation order

        // @odersky writes: Not sure we will be able to drop this. I forgot the reason why we dereference () functions,
        // but there must have been one. So I propose to leave old version in comments to be resurrected if the problem resurfaces.
        // @Eugene writes: this dates back to the days when one could only reify functions
        // hence, blocks were translated into nullary functions, so
        // presumably, it was useful to immediately evaluate them to get the result of a block
//        val result = jmeth.invoke(singleton, freeTerms map (sym => sym.asInstanceOf[FreeTermVar].value.asInstanceOf[AnyRef]): _*)
//        if (etpe.typeSymbol != FunctionClass(0)) result
//        else {
//          val applyMeth = result.getClass.getMethod("apply")
//          applyMeth.invoke(result)
//        }
        val (singleton, jmeth) = compileExpr(expr)
        jmeth.invoke(singleton, thunks map (_.asInstanceOf[AnyRef]): _*)
      }

      def showAttributed(tree: Tree, printTypes: Boolean = true, printIds: Boolean = true, printKinds: Boolean = false): String = {
        val saved1 = settings.printtypes.value
        val saved2 = settings.uniqid.value
        val saved3 = settings.Yshowsymkinds.value
        try {
          settings.printtypes.value = printTypes
          settings.uniqid.value = printIds
          settings.Yshowsymkinds.value = printKinds
          tree.toString
        } finally {
          settings.printtypes.value = saved1
          settings.uniqid.value = saved2
          settings.Yshowsymkinds.value = saved3
        }
      }

      // reporter doesn't accumulate errors, but the front-end does
      def throwIfErrors() = {
        if (frontEnd.hasErrors) {
          var msg = "reflective compilation has failed: " + EOL + EOL
          msg += frontEnd.infos map (_.msg) mkString EOL
          throw new ToolBoxError(ToolBox.this, msg)
        }
      }
    }

    // todo. is not going to work with quoted arguments with embedded whitespaces
    lazy val arguments = options.split(" ")

    lazy val virtualDirectory =
      (arguments zip arguments.tail) collect { case ("-d", dir) => dir } lastOption match {
        case Some(outDir) => scala.tools.nsc.io.AbstractFile.getDirectory(outDir)
        case None => new VirtualDirectory("(memory)", None)
      }

    lazy val compiler: ToolBoxGlobal = {
      try {
        val errorFn: String => Unit = msg => frontEnd.log(NoPosition, msg, frontEnd.ERROR)
        val command = new CompilerCommand(arguments.toList, errorFn)
        command.settings.outputDirs setSingleOutput virtualDirectory
        val instance = new ToolBoxGlobal(command.settings, new FrontEndToReporterProxy(frontEnd) { val settings = command.settings })
        if (frontEnd.hasErrors) {
          var msg = "reflective compilation has failed: cannot initialize the compiler: " + EOL + EOL
          msg += frontEnd.infos map (_.msg) mkString EOL
          throw new ToolBoxError(this, msg)
        }
        instance.phase = (new instance.Run).typerPhase // need to manually set a phase, because otherwise TypeHistory will crash
        instance
      } catch {
        case ex: Throwable =>
          var msg = "reflective compilation has failed: cannot initialize the compiler due to %s".format(ex.toString)
          throw new ToolBoxError(this, msg, ex)
      }
    }

    // @Eugene: how do I make this work without casts?
    // lazy val importer = compiler.mkImporter(self)
    lazy val importer = compiler.mkImporter(self).asInstanceOf[compiler.Importer { val from: self.type }]

    lazy val exporter = importer.reverse

    lazy val classLoader = new AbstractFileClassLoader(virtualDirectory, self.classLoader)

    def typeCheck(tree: Tree, expectedType: Type = WildcardType, freeTypes: Map[FreeType, Type] = Map[FreeType, Type](), silent: Boolean = false, withImplicitViewsDisabled: Boolean = false, withMacrosDisabled: Boolean = false): Tree = {
      if (compiler.settings.verbose.value) println("typing "+tree+", expectedType = "+expectedType+", freeTypes = "+freeTypes)
      var ctree: compiler.Tree = importer.importTree(tree)
      var cexpectedType: compiler.Type = importer.importType(expectedType)

      if (compiler.settings.verbose.value) println("substituting "+ctree+", expectedType = "+expectedType)
      val cfreeTypes: Map[compiler.FreeType, compiler.Type] = freeTypes map { case (k, v) => (importer.importSymbol(k).asInstanceOf[compiler.FreeType], importer.importType(v)) }
      ctree = compiler.substituteFreeTypes(ctree, cfreeTypes)
      cexpectedType = compiler.substituteFreeTypes(cexpectedType, cfreeTypes)

      if (compiler.settings.verbose.value) println("typing "+ctree+", expectedType = "+expectedType)
      val ttree: compiler.Tree = compiler.typeCheckExpr(ctree, cexpectedType, silent = silent, withImplicitViewsDisabled = withImplicitViewsDisabled, withMacrosDisabled = withMacrosDisabled)
      val rmttree = exporter.importTree(ttree)
      rmttree
    }

    def inferImplicitValue(pt: Type, silent: Boolean = true, withMacrosDisabled: Boolean = false): Tree =
      // todo. implement this
      ???

    def inferImplicitView(tree: Tree, from: Type, to: Type, silent: Boolean = true, withMacrosDisabled: Boolean = false, reportAmbiguous: Boolean = true): Tree =
      // todo. implement this
      ???

    def resetAllAttrs(tree: Tree): Tree = {
      val ctree: compiler.Tree = importer.importTree(tree)
      val ttree: compiler.Tree = compiler.resetAllAttrs(ctree)
      exporter.importTree(ttree)
    }

    def resetLocalAttrs(tree: Tree): Tree = {
      val ctree: compiler.Tree = importer.importTree(tree)
      val ttree: compiler.Tree = compiler.resetLocalAttrs(ctree)
      exporter.importTree(ttree)
    }

    def showAttributed(tree: Tree, printTypes: Boolean = true, printIds: Boolean = true, printKinds: Boolean = false): String =
      compiler.showAttributed(importer.importTree(tree), printTypes, printIds, printKinds)

    def runExpr(tree: Tree, freeTypes: Map[FreeType, Type] = Map[FreeType, Type]()): Any = {
      if (compiler.settings.verbose.value) println("running "+tree+", freeTypes = "+freeTypes)
      var ctree: compiler.Tree = importer.importTree(tree)

      if (compiler.settings.verbose.value) println("substituting "+ctree)
      val cfreeTypes: Map[compiler.FreeType, compiler.Type] = freeTypes map { case (k, v) => (importer.importSymbol(k).asInstanceOf[compiler.FreeType], importer.importType(v)) }
      ctree = compiler.substituteFreeTypes(ctree, cfreeTypes)

      if (compiler.settings.verbose.value) println("running "+ctree)
      compiler.runExpr(ctree)
    }

    class ToolBoxError(val toolBox: ToolBox, val message: String, val cause: Throwable = null) extends Throwable(message, cause)

    object ToolBoxError extends ToolBoxErrorExtractor {
      def unapply(error: ToolBoxError): Option[(ToolBox, String)] = Some((error.toolBox, error.message))
    }
  }
}
