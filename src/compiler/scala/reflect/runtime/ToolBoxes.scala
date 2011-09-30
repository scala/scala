package scala.reflect
package runtime

import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.ReflectGlobal
import scala.tools.nsc.CompilerCommand
import scala.tools.nsc.Global
import scala.tools.nsc.typechecker.Modes
import scala.tools.nsc.io.VirtualDirectory
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import reflect.{mirror => rm}
import scala.tools.nsc.util.FreshNameCreator
import scala.reflect.internal.Flags
import scala.tools.nsc.util.NoSourceFile
import java.lang.{Class => jClass}

trait ToolBoxes extends { self: Universe =>

  class ToolBox(val reporter: Reporter = new StoreReporter, val options: String = "") {

    class ToolBoxGlobal(settings: scala.tools.nsc.Settings, reporter: scala.tools.nsc.reporters.Reporter)
    extends ReflectGlobal(settings, reporter) {
      import definitions._

      private final val wrapperMethodName = "wrapper"

      private def isFree(t: Tree) = t.isInstanceOf[Ident] && t.symbol.isInstanceOf[FreeVar]

      def wrapInClass(expr: Tree, fvs: List[Symbol]): ClassDef = {
        val clazz = EmptyPackageClass.newAnonymousClass(NoPosition)
        clazz setInfo ClassInfoType(List(ObjectClass.tpe), new Scope, clazz)
        val meth = clazz.newMethod(NoPosition, wrapperMethodName)
        meth setFlag Flags.STATIC
        meth setInfo MethodType(meth.owner.newSyntheticValueParams(fvs map (_.tpe)), expr.tpe)
        clazz.info.decls enter meth
        val methdef = DefDef(meth, expr)
        val clazzdef = ClassDef(clazz, NoMods, List(List()), List(List()), List(methdef), NoPosition)
        clazzdef
      }

      def wrapInCompilationUnit(tree: Tree): CompilationUnit = {
        val unit = new CompilationUnit(NoSourceFile)
        unit.body = tree
        unit
      }

      def compileExpr(expr: Tree, fvs: List[Symbol]): String = {
        val cdef = wrapInClass(expr, fvs)
        val unit = wrapInCompilationUnit(cdef)
        val run = new Run
        run.compileUnits(List(unit), run.namerPhase)
        cdef.name.toString
      }

      def runExpr(expr: Tree): Any = {
        val fvs = (expr filter isFree map (_.symbol)).distinct
        val className = compileExpr(expr, fvs)
        val jclazz = jClass.forName(className, true, classLoader)
        val jmeth = jclazz.getDeclaredMethods.find(_.getName == wrapperMethodName).get
        jmeth.invoke(null, fvs map (sym => sym.asInstanceOf[FreeVar].value.asInstanceOf[AnyRef]): _*)
      }
    }

    lazy val virtualDirectory = new VirtualDirectory("(memory)", None)

    lazy val compiler: ToolBoxGlobal = {
      val command = new CompilerCommand(options.split(" ").toList, reporter.error(scala.tools.nsc.util.NoPosition, _))
      command.settings.outputDirs setSingleOutput virtualDirectory
      new ToolBoxGlobal(command.settings, reporter)
    }

    lazy val importer = new compiler.Importer {
      val from: self.type = self
    }

    lazy val exporter = importer.reverse

    lazy val classLoader = new AbstractFileClassLoader(virtualDirectory, getClass.getClassLoader)


    def typeCheck(tree: rm.Tree, expectedType: rm.Type): rm.Tree = {
      if (compiler.settings.verbose.value) println("typing "+tree+", pt = "+expectedType)
      val run = new compiler.Run
      compiler.phase = run.refchecksPhase
      val ctree: compiler.Tree = importer.importTree(tree.asInstanceOf[Tree])
      val pt: compiler.Type = importer.importType(expectedType.asInstanceOf[Type])
      val ttree: compiler.Tree = compiler.typer.typed(ctree, compiler.analyzer.EXPRmode, pt)
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
  }
}