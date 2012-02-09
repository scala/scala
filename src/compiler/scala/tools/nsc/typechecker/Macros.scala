package scala.tools.nsc
package typechecker

import symtab.Flags._
import scala.tools.nsc.util._
import scala.reflect.ReflectionUtils

trait Macros { self: Analyzer =>
  import global._
  import definitions._

  def macroMeth(mac: Symbol): Symbol = {
    var owner = mac.owner
    if (!owner.isModuleClass) owner = owner.companionModule.moduleClass
    owner.info.decl(nme.macroMethodName(mac.name))
  }

  def macroArgs(tree: Tree): (List[List[Tree]]) = tree match {
    case Apply(fn, args) =>
      macroArgs(fn) :+ args
    case TypeApply(fn, args) =>
      macroArgs(fn) :+ args
    case Select(qual, name) if !isStaticMacro(tree.symbol) =>
      List(List(qual))
    case _ =>
      List(List())
  }

  private def isStaticMacro(mac: Symbol): Boolean =
    mac.owner.isModuleClass

  /**
   *  The definition of the method implementing a macro. Example:
   *  Say we have in a class C
   *
   *    def macro foo[T](xs: List[T]): T = expr
   *
   *  Then the following macro method is generated for `foo`:
   *
   *    def defmacro$foo
   *           (_context: scala.reflect.macro.Context)
   *           (_this: _context.Tree)
   *           (T: _context.TypeTree)
   *           (xs: _context.Tree): _context.Tree = {
   *      import _context._  // this means that all methods of Context can be used unqualified in macro's body
   *      expr
   *    }
   *
   *  If `foo` is declared in an object, the second parameter list is () instead of (_this: _context.Tree).
   *  If macro has no type arguments, the third parameter list is omitted (it's not empty, but omitted altogether).
   *
   *  To find out the desugared representation of your particular macro, compile it with -Ydebug.
   */
  def macroMethDef(mdef: DefDef): Tree = {
    def paramDef(name: Name, tpt: Tree) = ValDef(Modifiers(PARAM), name, tpt, EmptyTree)
    val contextType = TypeTree(ReflectMacroContext.tpe)
    val globParamSec = List(paramDef(nme.macroContext, contextType))
    def globSelect(name: Name) = Select(Ident(nme.macroContext), name)
    def globTree = globSelect(tpnme.Tree)
    def globTypeTree = globSelect(tpnme.TypeTree)
    val thisParamSec = if (isStaticMacro(mdef.symbol)) List() else List(paramDef(nme.macroThis, globTree))
    def tparamInMacro(tdef: TypeDef) = paramDef(tdef.name.toTermName, globTypeTree)
    def vparamInMacro(vdef: ValDef): ValDef = paramDef(vdef.name, vdef.tpt match {
      case tpt @ AppliedTypeTree(hk, _) if treeInfo.isRepeatedParamType(tpt) => AppliedTypeTree(hk, List(globTree))
      case _ => globTree
    })
    def wrapImplicit(tree: Tree) = atPos(tree.pos) {
      // implicit hasn't proven useful so far, so I'm disabling it
      //val implicitDecl = ValDef(Modifiers(IMPLICIT), nme.macroContextImplicit, SingletonTypeTree(Ident(nme.macroContext)), Ident(nme.macroContext))
      val importGlob = Import(Ident(nme.macroContext), List(ImportSelector(nme.WILDCARD, -1, null, -1)))
      Block(List(importGlob), tree)
    }
    var formals = (mdef.vparamss map (_ map vparamInMacro))
    if (mdef.tparams.nonEmpty) formals = (mdef.tparams map tparamInMacro) :: formals

    atPos(mdef.pos) {
      new DefDef( // can't call DefDef here; need to find out why
        mods = mdef.mods &~ MACRO &~ OVERRIDE,
        name = nme.macroMethodName(mdef.name),
        tparams = List(),
        vparamss = globParamSec :: thisParamSec :: formals,
        tpt = globTree,
        wrapImplicit(mdef.rhs))
    }
  }

  def addMacroMethods(templ: Template, namer: Namer): Unit = {
    for (ddef @ DefDef(mods, _, _, _, _, _) <- templ.body if mods hasFlag MACRO) {
      val trace = scala.tools.nsc.util.trace when settings.debug.value
      val sym = namer.enterSyntheticSym(trace("macro def: ")(macroMethDef(ddef)))
      trace("added to "+namer.context.owner.enclClass+": ")(sym)
    }
  }

  lazy val mirror = new scala.reflect.runtime.Mirror {
    lazy val libraryClassLoader = {
      val classpath = global.classPath.asURLs
      ScalaClassLoader.fromURLs(classpath, self.getClass.getClassLoader)
    }

    override def defaultReflectiveClassLoader() = libraryClassLoader
  }

  /** Return optionally address of companion object and implementation method symbol
   *  of given macro; or None if implementation classfile cannot be loaded or does
   *  not contain the macro implementation.
   */
  def macroImpl(mac: Symbol): Option[(AnyRef, mirror.Symbol)] = {
    try {
      val mmeth = macroMeth(mac)
      if (mmeth == NoSymbol) None
      else {
        val receiverClass: mirror.Symbol = mirror.symbolForName(mmeth.owner.fullName)
        val receiverObj = receiverClass.companionModule
        if (receiverObj == mirror.NoSymbol) None
        else {
          val receiver = mirror.companionInstance(receiverClass)
          val rmeth = receiverObj.info.member(mirror.newTermName(mmeth.name.toString))
          if (rmeth == mirror.NoSymbol) None
          else {
            Some((receiver, rmeth))
          }
        }
      }
    } catch {
      case ex: ClassNotFoundException =>
        None
    }
  }

  /** Return result of macro expansion.
   *  Or, if that fails, and the macro overrides a method return
   *  tree that calls this method instead of the macro.
   */
  def macroExpand(tree: Tree, context: Context): Option[Any] = {
    val macroDef = tree.symbol
    macroImpl(macroDef) match {
      case Some((receiver, rmeth)) =>
        val argss = List(global) :: macroArgs(tree)
        val paramss = macroMeth(macroDef).paramss
        val rawArgss = for ((as, ps) <- argss zip paramss) yield {
          if (isVarArgsList(ps)) as.take(ps.length - 1) :+ as.drop(ps.length - 1)
          else as
        }
        val rawArgs: Seq[Any] = rawArgss.flatten
        val savedInfolevel = nodePrinters.infolevel
        try {
          // @xeno.by: InfoLevel.Verbose examines and prints out infos of symbols
          // by the means of this'es these symbols can climb up the lexical scope
          // when these symbols will be examined by a node printer
          // they will enumerate and analyze their children (ask for infos and tpes)
          // if one of those children involves macro expansion, things might get nasty
          // that's why I'm temporarily turning this behavior off
          nodePrinters.infolevel = nodePrinters.InfoLevel.Quiet
          Some(mirror.invoke(receiver, rmeth)(rawArgs: _*))
        } catch {
          case ex =>
            val realex = ReflectionUtils.unwrapThrowable(ex)
            val stacktrace = new java.io.StringWriter()
            realex.printStackTrace(new java.io.PrintWriter(stacktrace))
            val msg = System.getProperty("line.separator") + stacktrace
            context.unit.error(tree.pos, "exception during macro expansion: " + msg)
            None
        } finally {
          nodePrinters.infolevel = savedInfolevel
        }
      case None =>
        val trace = scala.tools.nsc.util.trace when settings.debug.value
        def notFound() = {
          context.unit.error(tree.pos, "macro implementation not found: " + macroDef.name)
          None
        }
        def fallBackToOverridden(tree: Tree): Option[Tree] = {
          tree match {
            case Select(qual, name) if (macroDef.isMacro) =>
              macroDef.allOverriddenSymbols match {
                case first :: _ =>
                  Some(Select(qual, name) setPos tree.pos setSymbol first)
                case _ =>
                  trace("macro is not overridden: ")(tree)
                  notFound()
              }
            case Apply(fn, args) =>
              fallBackToOverridden(fn) match {
                case Some(fn1) => Some(Apply(fn1, args) setPos tree.pos)
                case _         => None
              }
            case TypeApply(fn, args) =>
              fallBackToOverridden(fn) match {
                case Some(fn1) => Some(TypeApply(fn1, args) setPos tree.pos)
                case _         => None
              }
            case _ =>
              trace("unexpected tree in fallback: ")(tree)
              notFound()
          }
        }
        fallBackToOverridden(tree) match {
          case Some(tree1) =>
            trace("falling back to ")(tree1)
            currentRun.macroExpansionFailed = true
            Some(tree1)
          case None =>
            None
        }
    }
  }
}
