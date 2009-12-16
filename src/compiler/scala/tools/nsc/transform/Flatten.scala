/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Martin Odersky
 */
// $Id$

package scala.tools.nsc
package transform

import symtab._
import Flags._
import scala.collection.mutable.{HashMap, ListBuffer}

abstract class Flatten extends InfoTransform {
  import global._
  import definitions._

  /** the following two members override abstract members in Transform */
  val phaseName: String = "flatten"

  private def liftClass(sym: Symbol) {
    if (!(sym hasFlag LIFTED)) {
      sym setFlag LIFTED
      atPhase(phase.next) {
        if (settings.debug.value) log("re-enter " + sym + " in " + sym.owner)
        assert(sym.owner.isPackageClass, sym) //debug
        val scope = sym.owner.info.decls
        val old = scope lookup sym.name
        if (old != NoSymbol) scope unlink old
        scope enter sym
      }
    }
  }

  private val flattened = new TypeMap {
    def apply(tp: Type): Type = tp match {
      case TypeRef(pre, sym, args) if (pre.typeSymbol.isClass && !pre.typeSymbol.isPackageClass) =>
        assert(args.isEmpty)
        typeRef(sym.toplevelClass.owner.thisType, sym, args)
      case ClassInfoType(parents, decls, clazz) =>
        var parents1 = parents
        val decls1 = new Scope
        if (clazz.isPackageClass) {
          atPhase(phase.next)(decls.toList foreach (sym => decls1 enter sym))
        } else {
          val oldowner = clazz.owner
          atPhase(phase.next)(oldowner.info)
          parents1 = parents mapConserve (this)
          for (sym <- decls.toList) {
            if (sym.isTerm && !sym.isStaticModule) {
              decls1 enter sym
              if (sym.isModule) sym.moduleClass setFlag LIFTED
            } else if (sym.isClass) {
              liftClass(sym)
              if (sym.needsImplClass) liftClass(erasure.implClass(sym))
            }
          }
        }
        ClassInfoType(parents1, decls1, clazz)
      case MethodType(params, restp) =>
        val restp1 = apply(restp)
        if (restp1 eq restp) tp else copyMethodType(tp, params, restp1)
      case PolyType(tparams, restp) =>
        val restp1 = apply(restp);
        if (restp1 eq restp) tp else PolyType(tparams, restp1)
      case _ =>
        mapOver(tp)
    }
  }

  def transformInfo(sym: Symbol, tp: Type): Type = flattened(tp)

  protected def newTransformer(unit: CompilationUnit): Transformer = new Flattener

  class Flattener extends Transformer {

    /** Buffers for lifted out classes */
    private val liftedDefs = new HashMap[Symbol, ListBuffer[Tree]]

    override def transform(tree: Tree): Tree = {
      tree match {
        case PackageDef(_, _) =>
          liftedDefs(tree.symbol.moduleClass) = new ListBuffer
        case Template(_, _, _) if (tree.symbol.owner.hasFlag(PACKAGE)) =>
          liftedDefs(tree.symbol.owner) = new ListBuffer
        case _ =>
      }
      postTransform(super.transform(tree))
    }

    private def postTransform(tree: Tree): Tree = {
      val sym = tree.symbol
      val tree1 = tree match {
        case ClassDef(_, _, _, _) if sym.isNestedClass =>
          liftedDefs(sym.toplevelClass.owner) += tree
          EmptyTree
        case Select(qual, name) if (sym.isStaticModule && !sym.owner.isPackageClass) =>
          atPhase(phase.next) {
            atPos(tree.pos) {
              gen.mkAttributedRef(sym)
            }
          }
        case _ =>
          tree
      }
      tree1 setType flattened(tree1.tpe)
    }

    /** Transform statements and add lifted definitions to them. */
    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
      val stats1 = super.transformStats(stats, exprOwner)
      if (currentOwner.isPackageClass) stats1 ::: liftedDefs(currentOwner).toList
      else stats1
    }
  }
}
