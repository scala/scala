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
    val (lvdefs, gvdefs) = evdefs map {
      case vdef @ ValDef(mods, name, tpt, rhs) =>
        val fld = treeCopy.ValDef(
          vdef.duplicate, mods, name,
          atPos(focusPos(vdef.pos)) { TypeTree() setOriginal tpt setPos focusPos(tpt.pos) }, // atPos in case
          EmptyTree)
        val local = treeCopy.ValDef(vdef, Modifiers(PRESUPER), name, tpt, rhs)
        (local, fld)
    } unzip

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
    case TypeTreeWithDeferredRefCheck() => // TODO: should we traverse the wrapped tree?
      // (and rewrap the result? how to update the deferred check? would need to store wrapped tree instead of returning it from check)
    case _ => super.xtraverse(traverser, tree)
  }

  trait TreeCopier extends super.TreeCopierOps {
    def DocDef(tree: Tree, comment: DocComment, definition: Tree): DocDef
    def AssignOrNamedArg(tree: Tree, lhs: Tree, rhs: Tree): AssignOrNamedArg
    def SelectFromArray(tree: Tree, qualifier: Tree, selector: Name, erasure: Type): SelectFromArray
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
  def resetAllAttrs[A<:Tree](x:A): A = { new ResetAttrsTraverser().traverse(x); x }
  def resetLocalAttrs[A<:Tree](x:A): A = { new ResetLocalAttrsTraverser().traverse(x); x }

  /** A traverser which resets symbol and tpe fields of all nodes in a given tree
   *  except for (1) TypeTree nodes, whose <code>.tpe</code> field is kept, and
   *  (2) This(pkg) nodes, where pkg refers to a package symbol -- their attributes are kept, and
   *  (3) if a <code>.symbol</code> field refers to a symbol which is defined
   *  outside the tree, it is also kept.
   *
   *  (2) is necessary because some This(pkg) are generated where pkg is not
   *  an enclosing package.n In that case, resetting the symbol would cause the
   *  next type checking run to fail. See #3152.
   *
   *  (bq:) This traverser has mutable state and should be discarded after use
   */
  private class ResetAttrsTraverser extends Traverser {
    protected def isLocal(sym: Symbol): Boolean = true
    protected def resetDef(tree: Tree) {
      tree.symbol = NoSymbol
    }
    override def traverse(tree: Tree): Unit = {
      tree match {
        case _: DefTree | Function(_, _) | Template(_, _, _) =>
          resetDef(tree)
          tree.tpe = null
          tree match {
            case tree: DefDef => tree.tpt.tpe = null
            case _ => ()
          }
        case tpt: TypeTree =>
          if (tpt.wasEmpty) tree.tpe = null
        case This(_) if tree.symbol != null && tree.symbol.isPackageClass =>
          ;
        case EmptyTree =>
          ;
        case _ =>
          if (tree.hasSymbol && isLocal(tree.symbol)) tree.symbol = NoSymbol
          tree.tpe = null
      }
      super.traverse(tree)
    }
  }

  private class ResetLocalAttrsTraverser extends ResetAttrsTraverser {
    private val erasedSyms = util.HashSet[Symbol](8)
    override protected def isLocal(sym: Symbol) = erasedSyms(sym)
    override protected def resetDef(tree: Tree) {
      erasedSyms addEntry tree.symbol
      super.resetDef(tree)
    }
    override def traverse(tree: Tree): Unit = tree match {
      case Template(parents, self, body) =>
        for (stat <- body)
          if (stat.isDef) erasedSyms.addEntry(stat.symbol)
        super.traverse(tree)
      case _ =>
        super.traverse(tree)
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