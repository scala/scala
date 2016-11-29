/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package transform

import symtab._
import Flags._
import scala.collection.mutable.ListBuffer

abstract class Flatten extends InfoTransform {
  import global._
  import treeInfo.isQualifierSafeToElide

  /** the following two members override abstract members in Transform */
  val phaseName: String = "flatten"

  /** Updates the owning scope with the given symbol, unlinking any others.
   */
  private def replaceSymbolInCurrentScope(sym: Symbol): Unit = exitingFlatten {
    removeSymbolInCurrentScope(sym)
    sym.owner.info.decls enter sym
  }

  private def removeSymbolInCurrentScope(sym: Symbol): Unit = exitingFlatten {
    val scope = sym.owner.info.decls
    val old   = (scope lookupUnshadowedEntries sym.name).toList
    old foreach (scope unlink _)
    def old_s = old map (_.sym) mkString ", "
    if (old.nonEmpty) debuglog(s"In scope of ${sym.owner}, unlinked $old_s")
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
                if (sym.isModule) {
                  // In theory, we could assert(sym.isMethod), because nested, non-static modules are
                  // transformed to methods (METHOD flag added in UnCurry). But this requires
                  // forcing sym.info (see comment on isModuleNotMethod), which forces stub symbols
                  // too eagerly (SI-8907).

                  // Note that module classes are not entered into the 'decls' of the ClassInfoType
                  // of the outer class, only the module symbols are. So the current loop does
                  // not visit module classes. Therefore we set the LIFTED flag here for module
                  // classes.
                  // TODO: should we also set the LIFTED flag for static, nested module classes?
                  // currently they don't get the flag, even though they are lifted to the package
                  sym.moduleClass setFlag LIFTED
                }
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
        val restp1 = apply(restp)
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

    override def transform(tree: Tree): Tree = postTransform {
      tree match {
        case PackageDef(_, _) =>
          liftedDefs(tree.symbol.moduleClass) = new ListBuffer
          super.transform(tree)
        case Template(_, _, _) if tree.symbol.isDefinedInPackage =>
          liftedDefs(tree.symbol.owner) = new ListBuffer
          super.transform(tree)
        case ClassDef(_, _, _, _) if tree.symbol.isNestedClass =>
          // SI-5508 Ordering important. In `object O { trait A { trait B } }`, we want `B` to appear after `A` in
          //         the sequence of lifted trees in the enclosing package. Why does this matter? Currently, mixin
          //         needs to transform `A` first to a chance to create accessors for private[this] trait fields
          //         *before* it transforms inner classes that refer to them. This also fixes SI-6231.
          //
          //         Alternative solutions
          //            - create the private[this] accessors eagerly in Namer (but would this cover private[this] fields
          //              added later phases in compilation?)
          //            - move the accessor creation to the Mixin info transformer
          val liftedBuffer = liftedDefs(tree.symbol.enclosingTopLevelClass.owner)
          val index = liftedBuffer.length
          liftedBuffer.insert(index, super.transform(tree))
          if (tree.symbol.sourceModule.isStaticModule)
            removeSymbolInCurrentScope(tree.symbol.sourceModule)
          EmptyTree
        case _ =>
          super.transform(tree)
      }
    }

    private def postTransform(tree: Tree): Tree = {
      val sym = tree.symbol
      val tree1 = tree match {
        case Select(qual, name) if sym.isStaticModule && !sym.isTopLevel =>
          exitingFlatten {
            atPos(tree.pos) {
              val ref = gen.mkAttributedRef(sym)
              if (isQualifierSafeToElide(qual)) ref
              else Block(List(qual), ref).setType(tree.tpe) // need to execute the qualifier but refer directly to the lifted module.
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
      if (currentOwner.isPackageClass) {
        val lifted = liftedDefs.remove(currentOwner).toList.flatten
        stats1 ::: lifted
      }
      else stats1
    }
  }
}
