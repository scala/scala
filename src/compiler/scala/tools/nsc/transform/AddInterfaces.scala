/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package transform

import symtab._
import Flags._

abstract class AddInterfaces extends InfoTransform { self: Erasure =>
  import global._                  // the global environment
  import definitions._             // standard classes and methods

  def transformMixinInfo(tp: Type): Type = tp match {
    case ClassInfoType(parents, decls, clazz) if clazz.isPackageClass || !clazz.isJavaDefined =>

      val parents1 = parents match {
        case Nil      => Nil
        case hd :: tl =>
          assert(!hd.typeSymbol.isTrait, clazz)
          if (clazz.isTrait) ObjectTpe :: tl
          else parents
      }
      if (clazz.isTrait) {
        decls foreach { sym =>
          if (!sym.isType) sym.info // initialize to set lateMETHOD flag if necessary
        }
      }
      if (parents1 eq parents) tp
      else ClassInfoType(parents1, decls, clazz)
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

  /** Add calls to supermixin constructors
   *    `super[mix].$init$()`
   *  to tree, which is assumed to be the body of a constructor of class clazz.
   */
  private def addMixinConstructorCalls(tree: Tree, clazz: Symbol): Tree = {
    def mixinConstructorCall(mc: Symbol): Tree = atPos(tree.pos) {
      Apply(SuperSelect(clazz, mc.primaryConstructor), Nil)
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
