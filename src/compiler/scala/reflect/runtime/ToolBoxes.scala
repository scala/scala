package scala.reflect
package runtime

import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.reporters.AbstractReporter
import scala.tools.nsc.ReflectGlobal
import scala.tools.nsc.CompilerCommand
import scala.tools.nsc.Global
import scala.tools.nsc.typechecker.Modes
import scala.tools.nsc.io.VirtualDirectory
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import reflect.{mirror => rm}
import scala.tools.nsc.util.FreshNameCreator
import scala.reflect.internal.Flags
import scala.tools.nsc.util.{NoSourceFile, NoFile}
import java.lang.{Class => jClass}
import scala.tools.nsc.util.trace

trait ToolBoxes extends { self: Universe =>

  class ToolBox(val reporter: Reporter = new StoreReporter, val options: String = "") {

    class ToolBoxGlobal(settings: scala.tools.nsc.Settings, reporter: scala.tools.nsc.reporters.Reporter)
    extends ReflectGlobal(settings, reporter) {
      import definitions._

      private val trace = scala.tools.nsc.util.trace when settings.debug.value

      private final val wrapperMethodName = "wrapper"

      private var wrapCount = 0

      private def nextWrapperModuleName() = {
        wrapCount += 1
        newTermName("__wrapper$" + wrapCount)
      }

      private def moduleFileName(className: String) = className + "$"

      private def isFree(t: Tree) = t.isInstanceOf[Ident] && t.symbol.isInstanceOf[FreeVar]

      def typedTopLevelExpr(tree: Tree, pt: Type): Tree = {
        // !!! Why is this is in the empty package? If it's only to make
        // it inaccessible then please put it somewhere designed for that
        // rather than polluting the empty package with synthetics.
        trace("typing: ")(showAttributed(tree, true, true, settings.Yshowsymkinds.value))
        val ownerClass = EmptyPackageClass.newClassWithInfo(newTypeName("<expression-owner>"), List(ObjectClass.tpe), newScope)
        val owner      = ownerClass.newLocalDummy(tree.pos)
        val ttree = typer.atOwner(tree, owner).typed(tree, analyzer.EXPRmode, pt)
        trace("typed: ")(showAttributed(ttree, true, true, settings.Yshowsymkinds.value))
        ttree
      }

      def defOwner(tree: Tree): Symbol = tree find (_.isDef) map (_.symbol) match {
        case Some(sym) if sym != null && sym != NoSymbol => sym.owner
        case _ => NoSymbol
      }

      def wrapInObject(expr: Tree, fvs: List[Symbol]): ModuleDef = {
        val obj = EmptyPackageClass.newModule(nextWrapperModuleName())
        val minfo = ClassInfoType(List(ObjectClass.tpe, ScalaObjectClass.tpe), newScope, obj.moduleClass)
        obj.moduleClass setInfo minfo
        obj setInfo obj.moduleClass.tpe
        val meth = obj.moduleClass.newMethod(newTermName(wrapperMethodName))
        def makeParam(fv: Symbol) = meth.newValueParameter(fv.name.toTermName) setInfo fv.tpe
        meth setInfo MethodType(fvs map makeParam, AnyClass.tpe)
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
        val cleanedUp = resetLocalAttrs(moduledef)
        trace("cleaned up: ")(showAttributed(cleanedUp, true, true, settings.Yshowsymkinds.value))
        cleanedUp
      }

      def wrapInPackage(clazz: Tree): PackageDef =
        PackageDef(Ident(nme.EMPTY_PACKAGE_NAME), List(clazz))

      def wrapInCompilationUnit(tree: Tree): CompilationUnit = {
        val unit = new CompilationUnit(NoSourceFile)
        unit.body = tree
        unit
      }

      def compileExpr(expr: Tree, fvs: List[Symbol]): String = {
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
        if (!typed.isEmpty) {
          throw new Error("cannot compile trees that are already typed")
        }

        val mdef = wrapInObject(expr, fvs)
        val pdef = wrapInPackage(mdef)
        val unit = wrapInCompilationUnit(pdef)
        val run = new Run
        run.compileUnits(List(unit), run.namerPhase)
        mdef.symbol.fullName
      }

      private def getMethod(jclazz: jClass[_], name: String) =
        jclazz.getDeclaredMethods.find(_.getName == name).get

      def runExpr(expr: Tree): Any = {
        val fvs = (expr filter isFree map (_.symbol)).distinct

        reporter.reset()
        val className = compileExpr(expr, fvs)
        if (reporter.hasErrors) {
          throw new Error("reflective compilation has failed")
        }

        if (settings.debug.value) println("generated: "+className)
        val jclazz = jClass.forName(moduleFileName(className), true, classLoader)
        val jmeth = jclazz.getDeclaredMethods.find(_.getName == wrapperMethodName).get
        val jfield = jclazz.getDeclaredFields.find(_.getName == NameTransformer.MODULE_INSTANCE_NAME).get
        val singleton = jfield.get(null)
        // @odersky writes: Not sure we will be able to drop this. I forgot the reason why we dereference () functions,
        // but there must have been one. So I propose to leave old version in comments to be resurrected if the problem resurfaces.
//        val result = jmeth.invoke(singleton, fvs map (sym => sym.asInstanceOf[FreeVar].value.asInstanceOf[AnyRef]): _*)
//        if (etpe.typeSymbol != FunctionClass(0)) result
//        else {
//          val applyMeth = result.getClass.getMethod("apply")
//          applyMeth.invoke(result)
//        }
        jmeth.invoke(singleton, fvs map (sym => sym.asInstanceOf[FreeVar].value.asInstanceOf[AnyRef]): _*)
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
    }

    lazy val arguments = options.split(" ")

    lazy val virtualDirectory =
      (arguments zip arguments.tail) collect { case ("-d", dir) => dir } lastOption match {
        case Some(outDir) => scala.tools.nsc.io.AbstractFile.getDirectory(outDir)
        case None => new VirtualDirectory("(memory)", None)
      }

    lazy val compiler: ToolBoxGlobal = {
      val errorFn: String => Unit = reporter.error(scala.tools.nsc.util.NoPosition, _)
      val command = reporter match {
        case reporter: AbstractReporter => new CompilerCommand(arguments.toList, reporter.settings, errorFn)
        case _ => new CompilerCommand(arguments.toList, errorFn)
      }

      command.settings.outputDirs setSingleOutput virtualDirectory
      val instance = new ToolBoxGlobal(command.settings, reporter)

      // need to establish a run an phase because otherwise we run into an assertion in TypeHistory
      // that states that the period must be different from NoPeriod
      val run = new instance.Run
      instance.phase = run.refchecksPhase
      instance
    }

    lazy val importer = new compiler.Importer {
      val from: self.type = self
    }

    lazy val exporter = importer.reverse

    lazy val classLoader = new AbstractFileClassLoader(virtualDirectory, defaultReflectiveClassLoader)

    def typeCheck(tree: rm.Tree, expectedType: rm.Type): rm.Tree = {
      if (compiler.settings.verbose.value) println("typing "+tree+", pt = "+expectedType)
      val ctree: compiler.Tree = importer.importTree(tree.asInstanceOf[Tree])
      val pt: compiler.Type = importer.importType(expectedType.asInstanceOf[Type])
      val ttree: compiler.Tree = compiler.typedTopLevelExpr(ctree, pt)
      val rmttree = exporter.importTree(ttree).asInstanceOf[rm.Tree]
      rmttree
    }

    def typeCheck(tree: rm.Tree): rm.Tree =
      typeCheck(tree, WildcardType.asInstanceOf[rm.Type])

    def showAttributed(tree: rm.Tree, printTypes: Boolean = true, printIds: Boolean = true, printKinds: Boolean = false): String =
      compiler.showAttributed(importer.importTree(tree.asInstanceOf[Tree]), printTypes, printIds, printKinds)

    def runExpr(tree: rm.Tree): Any = {
      if (compiler.settings.verbose.value) println("running "+tree)
      val ctree: compiler.Tree = importer.importTree(tree.asInstanceOf[Tree])
      compiler.runExpr(ctree)
    }
  }
}
