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
        "__wrapper$" + wrapCount
      }

      private def moduleFileName(className: String) = className + "$"

      private def isFree(t: Tree) = t.isInstanceOf[Ident] && t.symbol.isInstanceOf[FreeVar]

      def wrapInObject(expr: Tree, fvs: List[Symbol]): ModuleDef = {
        val obj = EmptyPackageClass.newModule(NoPosition, nextWrapperModuleName())
        val minfo = ClassInfoType(List(ObjectClass.tpe), new Scope, obj.moduleClass)
        obj.moduleClass setInfo minfo
        obj setInfo obj.moduleClass.tpe
        val meth = obj.moduleClass.newMethod(NoPosition, wrapperMethodName)
        meth setFlag Flags.STATIC
        def makeParam(fv: Symbol) = meth.newValueParameter(NoPosition, fv.name) setInfo fv.tpe
        meth setInfo MethodType(fvs map makeParam, expr.tpe)
        minfo.decls enter meth
        val methdef = DefDef(meth, expr)
        val objdef = ModuleDef(
            obj,
            Template(
                List(TypeTree(ObjectClass.tpe)),
                emptyValDef,
                NoMods,
                List(),
                List(List()),
                List(methdef),
                NoPosition))
        resetAllAttrs(objdef)
      }

      def wrapInPackage(clazz: Tree): PackageDef =
        PackageDef(Ident(nme.EMPTY_PACKAGE_NAME), List(clazz))

      def wrapInCompilationUnit(tree: Tree): CompilationUnit = {
        val unit = new CompilationUnit(NoSourceFile)
        unit.body = tree
        unit
      }

      def compileExpr(expr: Tree, fvs: List[Symbol]): String = {
        val mdef = wrapInObject(expr, fvs)
        val pdef = trace("wrapped: ")(wrapInPackage(mdef))
        val unit = wrapInCompilationUnit(pdef)
        val run = new Run
        run.compileUnits(List(unit), run.namerPhase)
        mdef.symbol.fullName
      }

      private def getMethod(jclazz: jClass[_], name: String) =
        jclazz.getDeclaredMethods.find(_.getName == name).get

      def runExpr(expr: Tree): Any = {
        val etpe = expr.tpe
        val fvs = (expr filter isFree map (_.symbol)).distinct
        
        reporter.reset()
        val className = compileExpr(expr, fvs)
        if (reporter.hasErrors) {
          throw new Error("reflective compilation has failed")
        }
        
        if (settings.debug.value) println("generated: "+className)
        val jclazz = jClass.forName(moduleFileName(className), true, classLoader)
        val jmeth = jclazz.getDeclaredMethods.find(_.getName == wrapperMethodName).get
        val result = jmeth.invoke(null, fvs map (sym => sym.asInstanceOf[FreeVar].value.asInstanceOf[AnyRef]): _*)
        if (etpe.typeSymbol != FunctionClass(0)) result
        else {
          val applyMeth = result.getClass.getMethod("apply")
          applyMeth.invoke(result)
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
      new ToolBoxGlobal(command.settings, reporter)
    }

    lazy val importer = new compiler.Importer {
      val from: self.type = self
    }

    lazy val exporter = importer.reverse

    lazy val classLoader = new AbstractFileClassLoader(virtualDirectory, defaultReflectiveClassLoader)

    private def importAndTypeCheck(tree: rm.Tree, expectedType: rm.Type): compiler.Tree = {
      // need to establish a run an phase because otherwise we run into an assertion in TypeHistory
      // that states that the period must be different from NoPeriod
      val run = new compiler.Run
      compiler.phase = run.refchecksPhase
      val ctree: compiler.Tree = importer.importTree(tree.asInstanceOf[Tree])
      val pt: compiler.Type = importer.importType(expectedType.asInstanceOf[Type])
      val ttree: compiler.Tree = compiler.typer.typed(ctree, compiler.analyzer.EXPRmode, pt)
      ttree
    }

    def typeCheck(tree: rm.Tree, expectedType: rm.Type): rm.Tree = {
      if (compiler.settings.verbose.value) println("typing "+tree+", pt = "+expectedType)
      val ttree = importAndTypeCheck(tree, expectedType)
      exporter.importTree(ttree).asInstanceOf[rm.Tree]
    }

    def typeCheck(tree: rm.Tree): rm.Tree =
      typeCheck(tree, WildcardType.asInstanceOf[rm.Type])

    def showAttributed(tree: rm.Tree): String = {
      val saved = compiler.settings.printtypes.value
      try {
        compiler.settings.printtypes.value = true
        importer.importTree(tree.asInstanceOf[Tree]).toString
      } finally
        compiler.settings.printtypes.value = saved
    }

    def runExpr(tree: rm.Tree, expectedType: rm.Type): Any = {
      val ttree = importAndTypeCheck(tree, expectedType)
      compiler.runExpr(ttree)
    }

    def runExpr(tree: rm.Tree): Any = runExpr(tree, WildcardType.asInstanceOf[rm.Type])
  }
}
