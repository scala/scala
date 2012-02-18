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
    case Select(qual, name) =>
      List(List(qual))
    case _ =>
      List(List())
  }

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
   *  If macro has no type arguments, the third parameter list is omitted (it's not empty, but omitted altogether).
   *
   *  To find out the desugared representation of your particular macro, compile it with -Ymacro-debug.
   */
  def macroMethDef(mdef: DefDef): Tree = {
    def paramDef(name: Name, tpt: Tree) = ValDef(Modifiers(PARAM), name, tpt, EmptyTree)
    val contextType = TypeTree(ReflectMacroContext.tpe)
    val globParamSec = List(paramDef(nme.macroContext, contextType))
    def globSelect(name: Name) = Select(Ident(nme.macroContext), name)
    def globTree = globSelect(tpnme.Tree)
    def globTypeTree = globSelect(tpnme.TypeTree)
    val thisParamSec = List(paramDef(newTermName(nme.macroThis), globTree))
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
      val trace = scala.tools.nsc.util.trace when settings.Ymacrodebug.value
      val sym = namer.enterSyntheticSym(trace("macro def: ")(macroMethDef(ddef)))
      trace("added to "+namer.context.owner.enclClass+": ")(sym)
    }
  }

  lazy val mirror = new scala.reflect.runtime.Mirror {
    lazy val libraryClassLoader = {
      // todo. this is more or less okay, but not completely correct
      // see https://issues.scala-lang.org/browse/SI-5433 for more info
      val classpath = global.classPath.asURLs
      var loader: ClassLoader = ScalaClassLoader.fromURLs(classpath, self.getClass.getClassLoader)

      // an heuristic to detect REPL
      if (global.settings.exposeEmptyPackage.value) {
        import scala.tools.nsc.interpreter._
        val virtualDirectory = global.settings.outputDirs.getSingleOutput.get
        loader = new AbstractFileClassLoader(virtualDirectory, loader) {}
      }

      loader
    }

    override def defaultReflectiveClassLoader() = libraryClassLoader
  }

  /** Return optionally address of companion object and implementation method symbol
   *  of given macro; or None if implementation classfile cannot be loaded or does
   *  not contain the macro implementation.
   */
  def macroImpl(mac: Symbol): Option[(AnyRef, mirror.Symbol)] = {
    val debug = settings.Ymacrodebug.value
    val trace = scala.tools.nsc.util.trace when debug
    trace("looking for macro implementation: ")(mac.fullNameString)

    try {
      val mmeth = macroMeth(mac)
      trace("found implementation at: ")(mmeth.fullNameString)

      if (mmeth == NoSymbol) None
      else {
        trace("loading implementation class: ")(mmeth.owner.fullName)
        trace("classloader is: ")("%s of type %s".format(mirror.libraryClassLoader, mirror.libraryClassLoader.getClass))
        def inferClasspath(cl: ClassLoader) = cl match {
          case cl: java.net.URLClassLoader => "[" + (cl.getURLs mkString ",") + "]"
          case _ => "<unknown>"
        }
        trace("classpath is: ")(inferClasspath(mirror.libraryClassLoader))

        // @xeno.by: relies on the fact that macros can only be defined in static classes
        def classfile(sym: Symbol): String = {
          def recur(sym: Symbol): String = sym match {
            case sym if sym.owner.isPackageClass =>
              val suffix = if (sym.isModuleClass) "$" else ""
              sym.fullName + suffix
            case sym =>
              val separator = if (sym.owner.isModuleClass) "" else "$"
              recur(sym.owner) + separator + sym.javaSimpleName
          }

          if (sym.isClass || sym.isModule) recur(sym)
          else recur(sym.enclClass)
        }

        // @xeno.by: this doesn't work for inner classes
        // neither does mmeth.owner.javaClassName, so I had to roll my own implementation
        //val receiverName = mmeth.owner.fullName
        val receiverName = classfile(mmeth.owner)
        val receiverClass: mirror.Symbol = mirror.symbolForName(receiverName)

        if (debug) {
          println("receiverClass is: " + receiverClass.fullNameString)

          val jreceiverClass = mirror.classToJava(receiverClass)
          val jreceiverSource = jreceiverClass.getProtectionDomain.getCodeSource
          println("jreceiverClass is %s from %s".format(jreceiverClass, jreceiverSource))
          println("jreceiverClassLoader is %s with classpath %s".format(jreceiverClass.getClassLoader, inferClasspath(jreceiverClass.getClassLoader)))
        }

        val receiverObj = receiverClass.companionModule
        trace("receiverObj is: ")(receiverObj.fullNameString)

        if (receiverObj == mirror.NoSymbol) None
        else {
          // @xeno.by: yet another reflection method that doesn't work for inner classes
          //val receiver = mirror.companionInstance(receiverClass)
          val clazz = java.lang.Class.forName(receiverName, true, mirror.libraryClassLoader)
          val receiver = clazz getField "MODULE$" get null

          val rmeth = receiverObj.info.member(mirror.newTermName(mmeth.name.toString))
          if (debug) {
            println("rmeth is: " + rmeth.fullNameString)
            println("jrmeth is: " + mirror.methodToJava(rmeth))
          }

          if (rmeth == mirror.NoSymbol) None
          else {
            Some((receiver, rmeth))
          }
        }
      }
    } catch {
      case ex: ClassNotFoundException =>
        trace("implementation class failed to load: ")(ex.toString)
        None
    }
  }

  /** Return result of macro expansion.
   *  Or, if that fails, and the macro overrides a method return
   *  tree that calls this method instead of the macro.
   */
  def macroExpand(tree: Tree, typer: Typer): Option[Any] = {
    val trace = scala.tools.nsc.util.trace when settings.Ymacrodebug.value
    trace("macroExpand: ")(tree)

    val macroDef = tree.symbol
    macroImpl(macroDef) match {
      case Some((receiver, rmeth)) =>
        val argss = List(global) :: macroArgs(tree)
        val paramss = macroMeth(macroDef).paramss
        trace("paramss: ")(paramss)
        val rawArgss = for ((as, ps) <- argss zip paramss) yield {
          if (isVarArgsList(ps)) as.take(ps.length - 1) :+ as.drop(ps.length - 1)
          else as
        }
        val rawArgs: Seq[Any] = rawArgss.flatten
        trace("rawArgs: ")(rawArgs)
        val savedInfolevel = nodePrinters.infolevel
        try {
          // @xeno.by: InfoLevel.Verbose examines and prints out infos of symbols
          // by the means of this'es these symbols can climb up the lexical scope
          // when these symbols will be examined by a node printer
          // they will enumerate and analyze their children (ask for infos and tpes)
          // if one of those children involves macro expansion, things might get nasty
          // that's why I'm temporarily turning this behavior off
          nodePrinters.infolevel = nodePrinters.InfoLevel.Quiet
          val expanded = mirror.invoke(receiver, rmeth)(rawArgs: _*)
          expanded match {
            case expanded: Tree =>
              val expectedTpe = tree.tpe
              val typed = typer.typed(expanded, EXPRmode, expectedTpe)
              Some(typed)
            case expanded if expanded.isInstanceOf[Tree] =>
              typer.context.unit.error(tree.pos, "macro must return a compiler-specific tree; returned value is Tree, but it doesn't belong to this compiler's universe")
              None
            case expanded =>
              typer.context.unit.error(tree.pos, "macro must return a compiler-specific tree; returned value is of class: " + expanded.getClass)
              None
          }
        } catch {
          case ex =>
            val realex = ReflectionUtils.unwrapThrowable(ex)
            val msg = if (settings.Ymacrodebug.value) {
              val stacktrace = new java.io.StringWriter()
              realex.printStackTrace(new java.io.PrintWriter(stacktrace))
              System.getProperty("line.separator") + stacktrace
            } else {
              realex.getMessage
            }
            typer.context.unit.error(tree.pos, "exception during macro expansion: " + msg)
            None
        } finally {
          nodePrinters.infolevel = savedInfolevel
        }
      case None =>
        def notFound() = {
          typer.context.unit.error(tree.pos, "macro implementation not found: " + macroDef.name)
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
