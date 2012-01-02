/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package ast

import scala.reflect.internal.Flags.BYNAMEPARAM
import scala.reflect.internal.Flags.DEFAULTPARAM
import scala.reflect.internal.Flags.IMPLICIT
import scala.reflect.internal.Flags.PARAM
import scala.reflect.internal.Flags.PARAMACCESSOR
import scala.reflect.internal.Flags.PRESUPER
import scala.reflect.internal.Flags.TRAIT

trait Trees extends reflect.internal.Trees { self: Global =>

  // --- additional cases --------------------------------------------------------

  /** Only used during parsing */
  case class Parens(args: List[Tree]) extends Tree

  /** Documented definition, eliminated by analyzer */
  case class DocDef(comment: DocComment, definition: Tree)
       extends Tree {
    override def symbol: Symbol = definition.symbol
    override def symbol_=(sym: Symbol) { definition.symbol = sym }
    override def isDef = definition.isDef
    override def isTerm = definition.isTerm
    override def isType = definition.isType
  }


  /** Either an assignment or a named argument. Only appears in argument lists,
   *  eliminated by typecheck (doTypedApply)
   */
  case class AssignOrNamedArg(lhs: Tree, rhs: Tree)
       extends TermTree

 /** Array selection <qualifier> . <name> only used during erasure */
  case class SelectFromArray(qualifier: Tree, name: Name, erasure: Type)
       extends TermTree with RefTree { }

  /** emitted by typer, eliminated by refchecks */
  case class TypeTreeWithDeferredRefCheck()(val check: () => TypeTree) extends TypTree
  
  /** Marks underlying reference to id as boxed. 
   *  @pre: id must refer to a captured variable
   *  A reference such marked will refer to the boxed entity, no dereferencing
   *  with `.elem` is done on it.
   *  This tree node can be emitted by macros such as reify that call markBoxedReference.
   *  It is eliminated in LambdaLift, where the boxing conversion takes place.
   */
  case class ReferenceToBoxed(idt: Ident) extends TermTree

  // --- factory methods ----------------------------------------------------------

    /** Generates a template with constructor corresponding to
   *
   *  constrmods (vparams1_) ... (vparams_n) preSuper { presupers }
   *  extends superclass(args_1) ... (args_n) with mixins { self => body }
   *
   *  This gets translated to
   *
   *  extends superclass with mixins { self =>
   *    presupers' // presupers without rhs
   *    vparamss   // abstract fields corresponding to value parameters
   *    def <init>(vparamss) {
   *      presupers
   *      super.<init>(args)
   *    }
   *    body
   *  }
   */
  def Template(parents: List[Tree], self: ValDef, constrMods: Modifiers, vparamss: List[List[ValDef]], argss: List[List[Tree]], body: List[Tree], superPos: Position): Template = {
    /* Add constructor to template */

    // create parameters for <init> as synthetic trees.
    var vparamss1 =
      vparamss map (vps => vps.map { vd =>
        atPos(focusPos(vd.pos)) {
          ValDef(
            Modifiers(vd.mods.flags & (IMPLICIT | DEFAULTPARAM | BYNAMEPARAM) | PARAM | PARAMACCESSOR) withAnnotations vd.mods.annotations,
            vd.name, vd.tpt.duplicate, vd.rhs.duplicate)
        }})
    val (edefs, rest) = body span treeInfo.isEarlyDef
    val (evdefs, etdefs) = edefs partition treeInfo.isEarlyValDef
    val gvdefs = evdefs map {
      case vdef @ ValDef(mods, name, tpt, rhs) =>
        treeCopy.ValDef(
          vdef.duplicate, mods, name,
          atPos(focusPos(vdef.pos)) { TypeTree() setOriginal tpt setPos focusPos(tpt.pos) }, // atPos in case
          EmptyTree)
    }
    val lvdefs = evdefs map {
      case vdef @ ValDef(mods, name, tpt, rhs) =>
        treeCopy.ValDef(vdef, Modifiers(PRESUPER), name, tpt, rhs)
    }
    val constrs = {
      if (constrMods hasFlag TRAIT) {
        if (body forall treeInfo.isInterfaceMember) List()
        else List(
          atPos(wrappingPos(superPos, lvdefs)) (
            DefDef(NoMods, nme.MIXIN_CONSTRUCTOR, List(), List(List()), TypeTree(), Block(lvdefs, Literal(Constant())))))
      } else {
        // convert (implicit ... ) to ()(implicit ... ) if its the only parameter section
        if (vparamss1.isEmpty || !vparamss1.head.isEmpty && vparamss1.head.head.mods.isImplicit)
          vparamss1 = List() :: vparamss1;
        val superRef: Tree = atPos(superPos) {
          Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR)
        }
        val superCall = (superRef /: argss) (Apply)
        List(
          atPos(wrappingPos(superPos, lvdefs ::: argss.flatten)) (
            DefDef(constrMods, nme.CONSTRUCTOR, List(), vparamss1, TypeTree(), Block(lvdefs ::: List(superCall), Literal(Constant())))))
      }
    }
    // println("typed template, gvdefs = "+gvdefs+", parents = "+parents+", constrs = "+constrs)
    constrs foreach (ensureNonOverlapping(_, parents ::: gvdefs))
    // vparamss2 are used as field definitions for the class. remove defaults
    val vparamss2 = vparamss map (vps => vps map { vd =>
      treeCopy.ValDef(vd, vd.mods &~ DEFAULTPARAM, vd.name, vd.tpt, EmptyTree)
    })
    Template(parents, self, gvdefs ::: vparamss2.flatten ::: constrs ::: etdefs ::: rest)
  }

  /** Construct class definition with given class symbol, value parameters,
   *  supercall arguments and template body.
   *
   *  @param sym        the class symbol
   *  @param constrMods the modifiers for the class constructor, i.e. as in `class C private (...)`
   *  @param vparamss   the value parameters -- if they have symbols they
   *                    should be owned by `sym`
   *  @param argss      the supercall arguments
   *  @param body       the template statements without primary constructor
   *                    and value parameter fields.
   */
  def ClassDef(sym: Symbol, constrMods: Modifiers, vparamss: List[List[ValDef]], argss: List[List[Tree]], body: List[Tree], superPos: Position): ClassDef =
    ClassDef(sym,
      Template(sym.info.parents map TypeTree,
               if (sym.thisSym == sym || phase.erasedTypes) emptyValDef else ValDef(sym.thisSym),
               constrMods, vparamss, argss, body, superPos))

 // --- subcomponents --------------------------------------------------

  object treeInfo extends {
    val global: Trees.this.type = self
  } with TreeInfo

  lazy val treePrinter = newTreePrinter()

  // --- additional cases in operations ----------------------------------

  override protected def xtraverse(traverser: Traverser, tree: Tree): Unit = tree match {
    case Parens(ts) =>
      traverser.traverseTrees(ts)
    case DocDef(comment, definition) =>
      traverser.traverse(definition)
    case AssignOrNamedArg(lhs, rhs) =>
      traverser.traverse(lhs); traverser.traverse(rhs)
    case SelectFromArray(qualifier, selector, erasure) =>
      traverser.traverse(qualifier)
    case ReferenceToBoxed(idt) =>
      traverser.traverse(idt)
    case TypeTreeWithDeferredRefCheck() => // TODO: should we traverse the wrapped tree?
      // (and rewrap the result? how to update the deferred check? would need to store wrapped tree instead of returning it from check)
    case _ => super.xtraverse(traverser, tree)
  }

  trait TreeCopier extends super.TreeCopierOps {
    def DocDef(tree: Tree, comment: DocComment, definition: Tree): DocDef
    def AssignOrNamedArg(tree: Tree, lhs: Tree, rhs: Tree): AssignOrNamedArg
    def SelectFromArray(tree: Tree, qualifier: Tree, selector: Name, erasure: Type): SelectFromArray
    def ReferenceToBoxed(tree: Tree, idt: Ident): ReferenceToBoxed
    def TypeTreeWithDeferredRefCheck(tree: Tree): TypeTreeWithDeferredRefCheck
  }

  def newStrictTreeCopier: TreeCopier = new StrictTreeCopier
  def newLazyTreeCopier: TreeCopier = new LazyTreeCopier

  class StrictTreeCopier extends super.StrictTreeCopier with TreeCopier {
    def DocDef(tree: Tree, comment: DocComment, definition: Tree) =
      new DocDef(comment, definition).copyAttrs(tree)
    def AssignOrNamedArg(tree: Tree, lhs: Tree, rhs: Tree) =
      new AssignOrNamedArg(lhs, rhs).copyAttrs(tree)
    def SelectFromArray(tree: Tree, qualifier: Tree, selector: Name, erasure: Type) =
      new SelectFromArray(qualifier, selector, erasure).copyAttrs(tree)
    def ReferenceToBoxed(tree: Tree, idt: Ident) =
      new ReferenceToBoxed(idt).copyAttrs(tree)
    def TypeTreeWithDeferredRefCheck(tree: Tree) = tree match {
      case dc@TypeTreeWithDeferredRefCheck() => new TypeTreeWithDeferredRefCheck()(dc.check).copyAttrs(tree)
    }
  }

  class LazyTreeCopier extends super.LazyTreeCopier with TreeCopier {
    def DocDef(tree: Tree, comment: DocComment, definition: Tree) = tree match {
      case t @ DocDef(comment0, definition0)
      if (comment0 == comment) && (definition0 == definition) => t
      case _ => this.treeCopy.DocDef(tree, comment, definition)
    }
    def AssignOrNamedArg(tree: Tree, lhs: Tree, rhs: Tree) = tree match {
      case t @ AssignOrNamedArg(lhs0, rhs0)
      if (lhs0 == lhs) && (rhs0 == rhs) => t
      case _ => this.treeCopy.AssignOrNamedArg(tree, lhs, rhs)
    }
    def SelectFromArray(tree: Tree, qualifier: Tree, selector: Name, erasure: Type) = tree match {
      case t @ SelectFromArray(qualifier0, selector0, _)
      if (qualifier0 == qualifier) && (selector0 == selector) => t
      case _ => this.treeCopy.SelectFromArray(tree, qualifier, selector, erasure)
    }
    def ReferenceToBoxed(tree: Tree, idt: Ident) = tree match {
      case t @ ReferenceToBoxed(idt0) 
      if (idt0 == idt) => t
      case _ => this.treeCopy.ReferenceToBoxed(tree, idt)
    }
    def TypeTreeWithDeferredRefCheck(tree: Tree) = tree match {
      case t @ TypeTreeWithDeferredRefCheck() => t
      case _ => this.treeCopy.TypeTreeWithDeferredRefCheck(tree)
    }
  }

  class Transformer extends super.Transformer {
    def transformUnit(unit: CompilationUnit) {
      try unit.body = transform(unit.body)
      catch {
        case ex: Exception =>
          println("unhandled exception while transforming "+unit)
          throw ex
      }
    }
  }

  override protected def xtransform(transformer: super.Transformer, tree: Tree): Tree = tree match {
    case DocDef(comment, definition) =>
      transformer.treeCopy.DocDef(tree, comment, transformer.transform(definition))
    case AssignOrNamedArg(lhs, rhs) =>
     transformer.treeCopy.AssignOrNamedArg(tree, transformer.transform(lhs), transformer.transform(rhs))
    case SelectFromArray(qualifier, selector, erasure) =>
      transformer.treeCopy.SelectFromArray(
        tree, transformer.transform(qualifier), selector, erasure)
    case ReferenceToBoxed(idt) =>
      transformer.treeCopy.ReferenceToBoxed(
        tree, transformer.transform(idt) match { case idt1: Ident => idt1 })
    case TypeTreeWithDeferredRefCheck() =>
      transformer.treeCopy.TypeTreeWithDeferredRefCheck(tree)
  }

  object resetPos extends Traverser {
    override def traverse(t: Tree) {
      if (t != EmptyTree) t.setPos(NoPosition)
      super.traverse(t)
    }
  }

  /** resets symbol and tpe fields in a tree, @see ResetAttrsTraverse
   */
//  def resetAllAttrs[A<:Tree](x:A): A = { new ResetAttrsTraverser().traverse(x); x }
//  def resetLocalAttrs[A<:Tree](x:A): A = { new ResetLocalAttrsTraverser().traverse(x); x }
  
  def resetAllAttrs[A<:Tree](x:A): A = new ResetAttrsTransformer(false).transformPoly(x)
  def resetLocalAttrs[A<:Tree](x:A): A = new ResetAttrsTransformer(true).transformPoly(x)

  /** A transformer which resets symbol and tpe fields of all nodes in a given tree,
   *  with special treatment of:
   *    TypeTree nodes: are replaced by their original if it exists, otherwise tpe field is reset
   *                    to empty if it started out empty or refers to local symbols (which are erased).
   *    TypeApply nodes: are deleted if type arguments end up reverted to empty
   *    This(pkg) notes where pkg is a pckage: these are kept.
   *
   *  (bq:) This traverser has mutable state and should be discarded after use
   */
  private class ResetAttrsTransformer(localOnly: Boolean) extends Transformer {
    private val erasedSyms = util.HashSet[Symbol](8)
    private def resetDef(tree: Tree) {
      if (tree.symbol != null && tree.symbol != NoSymbol)
        erasedSyms addEntry tree.symbol
      tree.symbol = NoSymbol
    }
    override def transform(tree: Tree): Tree = super.transform {
      tree match {
        case Template(_, _, body) =>
          body foreach resetDef
          resetDef(tree)
          tree.tpe = null
          tree
        case _: DefTree | Function(_, _) | Template(_, _, _) =>
          resetDef(tree)
          tree.tpe = null
          tree
        case tpt: TypeTree =>
          if (tpt.original != null)
            tpt.original
          else if (tpt.tpe != null && (tpt.wasEmpty || (tpt.tpe exists (tp => erasedSyms contains tp.typeSymbol))))
            tpt.tpe = null
          tree
        case TypeApply(fn, args) if args map transform exists (_.isEmpty) =>
          fn
        case This(_) if tree.symbol != null && tree.symbol.isPackageClass =>
          tree
        case EmptyTree =>
          tree
        case _ =>
          if (tree.hasSymbol && (!localOnly || (erasedSyms contains tree.symbol))) 
            tree.symbol = NoSymbol
          tree.tpe = null
          tree
      }
    }
    def transformPoly[T <: Tree](x: T): T = {
      val x1 = transform(x)
      assert(x.getClass isInstance x1)
      x1.asInstanceOf[T]
    }
  }

  /* New pattern matching cases:

   case Parens(expr)                                               (only used during parsing)
   case DocDef(comment, defn) =>                                   (eliminated by typer)
   case AssignOrNamedArg(lhs, rhs) =>                              (eliminated by typer)
   case TypeTreeWithDeferredRefCheck() =>                          (created and eliminated by typer)
   case SelectFromArray(_, _, _) =>                                (created and eliminated by erasure)

  */

 }