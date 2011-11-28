package scala.tools.nsc
package typechecker

import symtab.Flags._

trait Macros { self: Analyzer =>
  import global._
  import definitions._

  def macroMethName(name: Name) =
    newTermName((if (name.isTypeName) "type" else "def") + "macro$" + name)

  def macroMeth(mac: Symbol): Symbol = {
    var owner = mac.owner
    if (!owner.isModuleClass) owner = owner.companionModule.moduleClass
    owner.info.decl(macroMethName(mac.name))
  }

  /**
   * The definition of the method implementing a macro. Example:
   *  Say we have in a class C
   *
   *    def macro foo[T](xs: List[T]): T = expr
   *
   *  Then the following macro method is generated for `foo`:
   *
   *    def defmacro$foo(glob: scala.reflect.api.Universe)
   *           (_this: glob.Tree)
   *           (T: glob.Type)
   *           (xs: glob.Tree): glob.Tree = {
   *      implicit val $glob = glob
   *      expr
   *    }
   *
   *    If `foo` is declared in an object, the second parameter list is () instead of (_this: glob.Tree).
   */
  def macroMethDef(mdef: DefDef): Tree = {
    def paramDef(name: Name, tpt: Tree) = ValDef(Modifiers(PARAM), name, tpt, EmptyTree)
    val universeType = TypeTree(ReflectApiUniverse.tpe)
    val globParamSec = List(paramDef("glob", universeType))
    def globSelect(name: Name) = Select(Ident("glob"), name)
    def globTree = globSelect(newTypeName("Tree"))
    def globType = globSelect(newTypeName("Type"))
    val thisParamSec = if (mdef.symbol.owner.isModuleClass) List() else List(paramDef("_this", globTree))
    def tparamInMacro(tdef: TypeDef) = paramDef(tdef.name.toTermName, globType)
    def vparamInMacro(vdef: ValDef): ValDef = paramDef(vdef.name, globTree)
    def wrapImplicit(tree: Tree) = atPos(tree.pos) {
      Block(List(ValDef(Modifiers(IMPLICIT), "$glob", universeType, Ident("glob"))), tree)
    }

    atPos(mdef.pos) {
      new DefDef( // can't call DefDef here; need to find out why
        mods = mdef.mods &~ MACRO,
        name = macroMethName(mdef.name),
        tparams = List(),
        vparamss = globParamSec :: thisParamSec :: (mdef.tparams map tparamInMacro) ::
          (mdef.vparamss map (_ map vparamInMacro)),
        tpt = globTree,
        wrapImplicit(mdef.rhs))
    }
  }

  def addMacroMethods(templ: Template, namer: Namer): Unit = {
    for (ddef @ DefDef(mods, _, _, _, _, _) <- templ.body if mods hasFlag MACRO) {
      val sym = namer.enterSyntheticSym(util.trace("macro def: ")(macroMethDef(ddef)))
      println("added to "+namer.context.owner.enclClass+": "+sym)
    }
  }

  def macroExpand(tree: Tree): Tree = ???

}