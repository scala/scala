/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package transform

import symtab._
import Flags._
import scala.collection.{ mutable, immutable }
import scala.collection.mutable.ListBuffer

abstract class Flatten extends InfoTransform {
  import global._
  import definitions._

  /** the following two members override abstract members in Transform */
  val phaseName: String = "flatten"

  /** Updates the owning scope with the given symbol; returns the old symbol.
   */
  private def replaceSymbolInCurrentScope(sym: Symbol): Symbol = exitingFlatten {
    val scope = sym.owner.info.decls
    val old   = scope lookup sym.name andAlso scope.unlink
    scope enter sym

    if (old eq NoSymbol)
      log(s"lifted ${sym.fullLocationString}")
    else
      log(s"lifted ${sym.fullLocationString} after unlinking existing $old from scope.")

    old
  }

  private def liftClass(sym: Symbol) {
    if (!sym.isLifted) {
      sym setFlag LIFTED
      debuglog("re-enter " + sym.fullLocationString)
      replaceSymbolInCurrentScope(sym)
    }
  }
  private def liftSymbol(sym: Symbol) {
    liftClass(sym)
    if (sym.needsImplClass)
      liftClass(erasure implClass sym)
  }
  // This is a short-term measure partially working around objects being
  // lifted out of parameterized classes, leaving them referencing
  // invisible type parameters.
  private def isFlattenablePrefix(pre: Type) = {
    val clazz = pre.typeSymbol
    clazz.isClass && !clazz.isPackageClass && {
      // Cannot flatten here: class A[T] { object B }
      // was "at erasurePhase.prev"
      enteringErasure(clazz.typeParams.isEmpty)
    }
  }

  private val flattened = new TypeMap {
    def apply(tp: Type): Type = tp match {
      case TypeRef(pre, sym, args) if isFlattenablePrefix(pre) =>
        assert(args.isEmpty && sym.enclosingTopLevelClass != NoSymbol, sym.ownerChain)
        typeRef(sym.enclosingTopLevelClass.owner.thisType, sym, Nil)
      case ClassInfoType(parents, decls, clazz) =>
        var parents1 = parents
        val decls1 = scopeTransform(clazz) {
          val decls1 = newScope
          if (clazz.isPackageClass) {
            exitingFlatten { decls foreach (decls1 enter _) }
          }
          else {
            val oldowner = clazz.owner
            exitingFlatten { oldowner.info }
            parents1 = parents mapConserve (this)

            for (sym <- decls) {
              if (sym.isTerm && !sym.isStaticModule) {
                decls1 enter sym
                if (sym.isModule)
                  sym.moduleClass setFlag LIFTED
              } else if (sym.isClass)
                liftSymbol(sym)
            }
          }
          decls1
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
    private val liftedDefs = perRunCaches.newMap[Symbol, ListBuffer[Tree]]()

    override def transform(tree: Tree): Tree = {
      tree match {
        case PackageDef(_, _) =>
          liftedDefs(tree.symbol.moduleClass) = new ListBuffer
        case Template(_, _, _) if tree.symbol.isDefinedInPackage =>
          liftedDefs(tree.symbol.owner) = new ListBuffer
        case _ =>
      }
      postTransform(super.transform(tree))
    }

    private def postTransform(tree: Tree): Tree = {
      val sym = tree.symbol
      val tree1 = tree match {
        case ClassDef(_, _, _, _) if sym.isNestedClass =>
          liftedDefs(sym.enclosingTopLevelClass.owner) += tree
          EmptyTree
        case Select(qual, name) if (sym.isStaticModule && !sym.owner.isPackageClass) =>
          exitingFlatten(atPos(tree.pos)(gen.mkAttributedRef(sym)))
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
