/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package transform

import symtab._
import Flags._
import scala.tools.nsc.util.ClassPath

abstract class AddInterfaces extends InfoTransform { self: Erasure =>
  import global._                  // the global environment
  import definitions._             // standard classes and methods

  /** lateDEFERRED for formerly concrete methods in such traits.
   */
  override def phaseNewFlags: Long = lateDEFERRED

  /** Is given trait member symbol a member of the trait's interface
   *  after this transform is performed?
   */
  def isInterfaceMember(sym: Symbol) = (
    sym.isType || {
      sym.info  // initialize to set lateMETHOD flag if necessary

      (     sym.isMethod
        && !sym.isLabel
      )
    }
  )

  /** Does symbol need an implementation method? */
  def needsImplMethod(sym: Symbol) = (
       sym.isMethod
    && isInterfaceMember(sym)
    && (!sym.hasFlag(DEFERRED | SUPERACCESSOR) || (sym hasFlag lateDEFERRED))
  )

  def implClassPhase = currentRun.erasurePhase.next

  def transformMixinInfo(tp: Type): Type = tp match {
    case ClassInfoType(parents, decls, clazz) if clazz.isPackageClass || !clazz.isJavaDefined =>

      val parents1 = parents match {
        case Nil      => Nil
        case hd :: tl =>
          assert(!hd.typeSymbol.isTrait, clazz)
          if (clazz.isTrait) ObjectTpe :: tl
          else parents
      }
      val decls1 = scopeTransform(clazz) {
        decls filter { sym =>
          if (clazz.isTrait) {
            isInterfaceMember(sym) || (sym.isTerm)
          } else sym.isClass || sym.isTerm
        }
      }
      //      if (clazz.isTrait) {
//        decls1.enter(clazz.newMethod(nme.MIXIN_CONSTRUCTOR, clazz.pos, Flags.PROTECTED | Flags.ARTIFACT) setInfo MethodType(Nil, UnitTpe))
//      }
      ClassInfoType(parents1, decls1, clazz)
    case _ =>
      tp
  }

// Tree transformation --------------------------------------------------------------
  private class ChangeOwnerAndReturnTraverser(oldowner: Symbol, newowner: Symbol)
    extends ChangeOwnerTraverser(oldowner, newowner) {
    override def traverse(tree: Tree) {
      tree match {
        case _: Return => change(tree.symbol)
        case _         =>
      }
      super.traverse(tree)
    }
  }

  private def mkAssign(clazz: Symbol, assignSym: Symbol, rhs: Tree): Tree = {
    val qual = Select(This(clazz), assignSym)
    if (assignSym.isSetter) Apply(qual, List(rhs))
    else Assign(qual, rhs)
  }

  /** Add calls to supermixin constructors
   *    `super[mix].$init$()`
   *  to tree, which is assumed to be the body of a constructor of class clazz.
   */
  private def addMixinConstructorCalls(tree: Tree, clazz: Symbol): Tree = {
    def mixinConstructorCall(mc: Symbol): Tree = atPos(tree.pos) {
      Apply(Select(Super(clazz, tpnme.EMPTY), mc.primaryConstructor), List())
    }
    val mixinConstructorCalls: List[Tree] = {
      for (mc <- clazz.mixinClasses.reverse
           if mc.isTrait && mc.primaryConstructor != NoSymbol)
      yield mixinConstructorCall(mc)
    }
    tree match {
      case Block(Nil, expr) =>
        // AnyVal constructor - have to provide a real body so the
        // jvm doesn't throw a VerifyError. But we can't add the
        // body until now, because the typer knows that Any has no
        // constructor and won't accept a call to super.init.
        assert((clazz isSubClass AnyValClass) || clazz.info.parents.isEmpty, clazz)
        Block(List(Apply(gen.mkSuperInitCall, Nil)), expr)

      case Block(stats, expr) =>
        // needs `hasSymbolField` check because `supercall` could be a block (named / default args)
        val (presuper, supercall :: rest) = stats span (t => t.hasSymbolWhich(_ hasFlag PRESUPER))
        treeCopy.Block(tree, presuper ::: (supercall :: mixinConstructorCalls ::: rest), expr)
    }
  }

  protected val mixinTransformer = new Transformer {
    override def transform(tree: Tree): Tree = {
      val sym = tree.symbol
      val tree1 = tree match {
//        case cd @ ClassDef(mods, _, _, impl) if sym.isTrait =>
//          val derived = deriveClassDef(cd)(templ => deriveTemplate(templ)(ts => ts.filter(t => !t.isDef || isInterfaceMember(t.symbol))))
//          derived
        case DefDef(_,_,_,_,_,_) if sym.isClassConstructor && sym.isPrimaryConstructor && sym.owner != ArrayClass =>
          deriveDefDef(tree)(addMixinConstructorCalls(_, sym.owner)) // (3)
        case Template(parents, self, body) =>
          val parents1 = sym.owner.info.parents map (t => TypeTree(t) setPos tree.pos)
          treeCopy.Template(tree, parents1, noSelfType, body)
        case _ =>
          tree
      }
      super.transform(tree1)
    }
  }
}
