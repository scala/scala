package scala.tools
package reflect

import scala.tools.nsc.reporters._
import scala.tools.nsc.CompilerCommand
import scala.tools.nsc.Global
import scala.tools.nsc.typechecker.Modes
import scala.tools.nsc.io.VirtualDirectory
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import scala.tools.nsc.util.FreshNameCreator
import scala.reflect.internal.Flags
import scala.reflect.internal.util.{BatchSourceFile, NoSourceFile, NoFile}
import java.lang.{Class => jClass}
import scala.compat.Platform.EOL
import scala.reflect.NameTransformer
import scala.reflect.api.JavaUniverse
import scala.reflect.base.MirrorOf

// [Eugene++ to Martin] by the way, toolboxes are unable to compile anything that involves packages
// is this intentional?

abstract class ToolBoxFactory[U <: JavaUniverse](val u: U) { factorySelf =>

  val mirror: u.Mirror

  def mkToolBox(frontEnd: FrontEnd = mkSilentFrontEnd(), options: String = ""): ToolBox[U] =
    new ToolBoxImpl(frontEnd, options)

  private class ToolBoxImpl(val frontEnd: FrontEnd, val options: String) extends ToolBox[U] { toolBoxSelf =>

    val u: factorySelf.u.type = factorySelf.u
    val mirror: u.Mirror = factorySelf.mirror

    class ToolBoxGlobal(settings: scala.tools.nsc.Settings, reporter: Reporter)
    extends ReflectGlobal(settings, reporter, toolBoxSelf.classLoader) {
      import definitions._

      private val trace = scala.tools.nsc.util.trace when settings.debug.value

      private var wrapCount = 0

      private final val wrapperMethodName = "wrapper"

      private def nextWrapperModuleName() = {
        wrapCount += 1
        // we need to use UUIDs here, because our toolbox might be spawned by another toolbox
        // that already has, say, __wrapper$1 in its virtual directory, which will shadow our codegen
        newTermName("__wrapper$" + wrapCount + "$" + java.util.UUID.randomUUID.toString.replace("-", ""))
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
        if (!typed.isEmpty) throw ToolBoxError("reflective toolbox has failed: cannot operate on trees that are already typed")

        val freeTypes = expr.freeTypes
        if (freeTypes.length > 0) {
          var msg = "reflective toolbox has failed:" + EOL
          msg += "unresolved free type variables (namely: " + (freeTypes map (ft => "%s %s".format(ft.name, ft.origin)) mkString ", ") + "). "
          msg += "have you forgot to use TypeTag annotations for type parameters external to a reifee? "
          msg += "if you have troubles tracking free type variables, consider using -Xlog-free-types"
          throw ToolBoxError(msg)
        }
      }

      def extractFreeTerms(expr0: Tree, wrapFreeTermRefs: Boolean): (Tree, collection.mutable.LinkedHashMap[FreeTermSymbol, TermName]) = {
        val freeTerms = expr0.freeTerms
        val freeTermNames = collection.mutable.LinkedHashMap[FreeTermSymbol, TermName]()
        freeTerms foreach (ft => {
          var name = ft.name.toString
          val namesakes = freeTerms takeWhile (_ != ft) filter (ft2 => ft != ft2 && ft.name == ft2.name)
          if (namesakes.length > 0) name += ("$" + (namesakes.length + 1))
          freeTermNames += (ft -> newTermName(name + nme.REIFY_FREE_VALUE_SUFFIX))
        })
        var expr = new Transformer {
          override def transform(tree: Tree): Tree =
            if (tree.hasSymbol && tree.symbol.isFreeTerm) {
              tree match {
                case Ident(_) =>
                  val freeTermRef = Ident(freeTermNames(tree.symbol.asFreeTermSymbol))
                  if (wrapFreeTermRefs) Apply(freeTermRef, List()) else freeTermRef
                case _ =>
                  throw new Error("internal error: %s (%s, %s) is not supported".format(tree, tree.productPrefix, tree.getClass))
              }
            } else {
              super.transform(tree)
            }
        }.transform(expr0)
        (expr, freeTermNames)
      }

      def typeCheckExpr(expr0: Tree, pt: Type, silent: Boolean = false, withImplicitViewsDisabled: Boolean, withMacrosDisabled: Boolean): Tree = {
        verifyExpr(expr0)

        // need to wrap the expr, because otherwise you won't be able to typecheck macros against something that contains free vars
        var (expr, freeTerms) = extractFreeTerms(expr0, wrapFreeTermRefs = false)
        val dummies = freeTerms.map{ case (freeTerm, name) => ValDef(NoMods, name, TypeTree(freeTerm.info), Select(Ident(PredefModule), newTermName("$qmark$qmark$qmark"))) }.toList
        expr = Block(dummies, expr)

        // [Eugene] how can we implement that?
        // !!! Why is this is in the empty package? If it's only to make
        // it inaccessible then please put it somewhere designed for that
        // rather than polluting the empty package with synthetics.
        val ownerClass    = rootMirror.EmptyPackageClass.newClassSymbol(newTypeName("<expression-owner>"))
        build.setTypeSignature(ownerClass, ClassInfoType(List(ObjectClass.tpe), newScope, ownerClass))
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
            var (dummies, unwrapped) = result match {
              case Block(dummies, unwrapped) => (dummies, unwrapped)
              case unwrapped => (Nil, unwrapped)
            }
            var invertedIndex = freeTerms map (_.swap)
            // todo. also fixup singleton types
            unwrapped = new Transformer {
              override def transform(tree: Tree): Tree =
                tree match {
                  case Ident(name) if invertedIndex contains name =>
                    Ident(invertedIndex(name)) setType tree.tpe
                  case _ =>
                    super.transform(tree)
                }
            }.transform(unwrapped)
            new TreeTypeSubstituter(dummies map (_.symbol), dummies map (dummy => SingleType(NoPrefix, invertedIndex(dummy.symbol.name)))).traverse(unwrapped)
            unwrapped
          case error @ analyzer.SilentTypeError(_) =>
            trace("failed: ")(error.err.errMsg)
            if (!silent) throw ToolBoxError("reflective typecheck has failed: %s".format(error.err.errMsg))
            EmptyTree
        })
      }

      def compileExpr(expr: Tree): (Object, java.lang.reflect.Method) = {
        verifyExpr(expr)

        def wrapExpr(expr0: Tree): Tree = {
          val (expr, freeTerms) = extractFreeTerms(expr0, wrapFreeTermRefs = true)

          val (obj, mclazz) = rootMirror.EmptyPackageClass.newModuleAndClassSymbol(
            nextWrapperModuleName())

          val minfo = ClassInfoType(List(ObjectClass.tpe), newScope, obj.moduleClass)
          obj.moduleClass setInfo minfo
          obj setInfo obj.moduleClass.tpe

          val meth = obj.moduleClass.newMethod(newTermName(wrapperMethodName))
          def makeParam(schema: (FreeTermSymbol, TermName)) = {
            val (fv, name) = schema
            // [Eugene] conventional way of doing this?
            val underlying = fv.tpe.resultType
            val tpe = appliedType(definitions.FunctionClass(0).tpe, List(underlying))
            meth.newValueParameter(name) setInfo tpe
          }
          meth setInfo MethodType(freeTerms.map(makeParam).toList, AnyClass.tpe)
          minfo.decls enter meth
          def defOwner(tree: Tree): Symbol = tree find (_.isDef) map (_.symbol) match {
            case Some(sym) if sym != null && sym != NoSymbol => sym.owner
            case _ => NoSymbol
          }
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

      def runExpr(expr: Tree): Any = {
        val freeTerms = expr.freeTerms // need to calculate them here, because later on they will be erased
        val thunks = freeTerms map (fte => () => fte.value) // need to be lazy in order not to distort evaluation order

        // @odersky writes: Not sure we will be able to drop this. I forgot the reason why we dereference () functions,
        // but there must have been one. So I propose to leave old version in comments to be resurrected if the problem resurfaces.
        // @Eugene writes: this dates back to the days when one could only reify functions
        // hence, blocks were translated into nullary functions, so
        // presumably, it was useful to immediately evaluate them to get the result of a block
        // @Eugene writes: anyways, I'll stash the old sources here in comments in case anyone wants to revive them
        // val result = jmeth.invoke(singleton, freeTerms map (sym => sym.asInstanceOf[FreeTermVar].value.asInstanceOf[AnyRef]): _*)
        // if (etpe.typeSymbol != FunctionClass(0)) result
        // else {
        //   val applyMeth = result.getClass.getMethod("apply")
        //   applyMeth.invoke(result)
        // }
        val (singleton, jmeth) = compileExpr(expr)
        val result = jmeth.invoke(singleton, thunks map (_.asInstanceOf[AnyRef]): _*)
        result
      }

      def parseExpr(code: String): Tree = {
        val run = new Run
        reporter.reset()
        val wrappedCode = "object wrapper {" + EOL + code + EOL + "}"
        val file = new BatchSourceFile("<toolbox>", wrappedCode)
        val unit = new CompilationUnit(file)
        phase = run.parserPhase
        val parser = new syntaxAnalyzer.UnitParser(unit)
        val wrappedTree = parser.parse()
        throwIfErrors()
        val PackageDef(_, List(ModuleDef(_, _, Template(_, _, _ :: parsed)))) = wrappedTree
        parsed match {
          case expr :: Nil => expr
          case stats :+ expr => Block(stats, expr)
        }
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
          throw ToolBoxError(msg)
        }
      }
    }

    // todo. is not going to work with quoted arguments with embedded whitespaces
    lazy val arguments = options.split(" ")

    lazy val virtualDirectory =
      (arguments zip arguments.tail).collect{ case ("-d", dir) => dir }.lastOption match {
        case Some(outDir) => scala.tools.nsc.io.AbstractFile.getDirectory(outDir)
        case None => new VirtualDirectory("(memory)", None)
      }

    lazy val compiler: ToolBoxGlobal = {
      try {
        val errorFn: String => Unit = msg => frontEnd.log(scala.reflect.internal.util.NoPosition, msg, frontEnd.ERROR)
        val command = new CompilerCommand(arguments.toList, errorFn)
        command.settings.outputDirs setSingleOutput virtualDirectory
        val instance = new ToolBoxGlobal(command.settings, new FrontEndToReporterProxy(frontEnd) { val settings = command.settings })
        if (frontEnd.hasErrors) {
          var msg = "reflective compilation has failed: cannot initialize the compiler: " + EOL + EOL
          msg += frontEnd.infos map (_.msg) mkString EOL
          throw ToolBoxError(msg)
        }
        instance
      } catch {
        case ex: Throwable =>
          var msg = "reflective compilation has failed: cannot initialize the compiler due to %s".format(ex.toString)
          throw ToolBoxError(msg, ex)
      }
    }

    lazy val importer = compiler.mkImporter(u)
    lazy val exporter = importer.reverse
    lazy val classLoader = new AbstractFileClassLoader(virtualDirectory, mirror.classLoader)

    def typeCheck(tree: u.Tree, expectedType: u.Type, freeTypes: Map[u.FreeTypeSymbol, u.Type], silent: Boolean, withImplicitViewsDisabled: Boolean, withMacrosDisabled: Boolean): u.Tree = {
      if (compiler.settings.verbose.value) println("typing "+tree+", expectedType = "+expectedType+", freeTypes = "+freeTypes)
      var ctree: compiler.Tree = importer.importTree(tree)
      var cexpectedType: compiler.Type = importer.importType(expectedType)

      if (compiler.settings.verbose.value) println("substituting "+ctree+", expectedType = "+expectedType)
      val cfreeTypes: Map[compiler.FreeTypeSymbol, compiler.Type] = freeTypes map { case (k, v) => (importer.importSymbol(k).asInstanceOf[compiler.FreeTypeSymbol], importer.importType(v)) }
      ctree = ctree.substituteTypes(cfreeTypes.keys.toList, cfreeTypes.values.toList)
      cexpectedType = cexpectedType.substituteTypes(cfreeTypes.keys.toList, cfreeTypes.values.toList)

      if (compiler.settings.verbose.value) println("typing "+ctree+", expectedType = "+expectedType)
      val ttree: compiler.Tree = compiler.typeCheckExpr(ctree, cexpectedType, silent = silent, withImplicitViewsDisabled = withImplicitViewsDisabled, withMacrosDisabled = withMacrosDisabled)
      val uttree = exporter.importTree(ttree)
      uttree
    }

    def inferImplicitValue(pt: u.Type, silent: Boolean, withMacrosDisabled: Boolean): u.Tree =
      // todo. implement this
      ???

    def inferImplicitView(tree: u.Tree, from: u.Type, to: u.Type, silent: Boolean, withMacrosDisabled: Boolean, reportAmbiguous: Boolean): u.Tree =
      // todo. implement this
      ???

    def resetAllAttrs(tree: u.Tree): u.Tree = {
      val ctree: compiler.Tree = importer.importTree(tree)
      val ttree: compiler.Tree = compiler.resetAllAttrs(ctree)
      val uttree = exporter.importTree(ttree)
      uttree
    }

    def resetLocalAttrs(tree: u.Tree): u.Tree = {
      val ctree: compiler.Tree = importer.importTree(tree)
      val ttree: compiler.Tree = compiler.resetLocalAttrs(ctree)
      val uttree = exporter.importTree(ttree)
      uttree
    }

    def showAttributed(tree: u.Tree, printTypes: Boolean = true, printIds: Boolean = true, printKinds: Boolean = false): String =
      compiler.showAttributed(importer.importTree(tree), printTypes, printIds, printKinds)

    def parseExpr(code: String): u.Tree = {
      if (compiler.settings.verbose.value) println("parsing "+code)
      val ctree: compiler.Tree = compiler.parseExpr(code)
      val utree = exporter.importTree(ctree)
      utree
    }

    def runExpr(tree: u.Tree, freeTypes: Map[u.FreeTypeSymbol, u.Type]): Any = {
      if (compiler.settings.verbose.value) println("running "+tree+", freeTypes = "+freeTypes)
      var ctree: compiler.Tree = importer.importTree(tree)

      if (compiler.settings.verbose.value) println("substituting "+ctree)
      val cfreeTypes: Map[compiler.FreeTypeSymbol, compiler.Type] = freeTypes map { case (k, v) => (importer.importSymbol(k).asInstanceOf[compiler.FreeTypeSymbol], importer.importType(v)) }
      ctree = ctree.substituteTypes(cfreeTypes.keys.toList, cfreeTypes.values.toList)

      if (compiler.settings.verbose.value) println("running "+ctree)
      compiler.runExpr(ctree)
    }
  }
}

