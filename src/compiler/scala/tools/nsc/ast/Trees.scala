/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
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
import scala.compat.Platform.EOL

trait Trees extends scala.reflect.internal.Trees { self: Global =>
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

 /** Array selection `<qualifier> . <name>` only used during erasure */
  case class SelectFromArray(qualifier: Tree, name: Name, erasure: Type)
       extends RefTree with TermTree

  /** Derived value class injection (equivalent to: `new C(arg)` after erasure); only used during erasure.
   *  The class `C` is stored as a tree attachment.
   */
  case class InjectDerivedValue(arg: Tree)
       extends SymTree with TermTree

  class PostfixSelect(qual: Tree, name: Name) extends Select(qual, name)

  /** emitted by typer, eliminated by refchecks */
  case class TypeTreeWithDeferredRefCheck()(val check: () => TypeTree) extends TypTree

  // --- factory methods ----------------------------------------------------------

  /** Factory method for a primary constructor super call `super.<init>(args_1)...(args_n)`
   */
  def PrimarySuperCall(argss: List[List[Tree]]): Tree = argss match {
    case Nil        => Apply(gen.mkSuperInitCall, Nil)
    case xs :: rest => rest.foldLeft(Apply(gen.mkSuperInitCall, xs): Tree)(Apply.apply)
  }

  /** Construct class definition with given class symbol, value parameters,
   *  supercall arguments and template body.
   *
   *  @param sym        the class symbol
   *  @param constrMods the modifiers for the class constructor, i.e. as in `class C private (...)`
   *  @param vparamss   the value parameters -- if they have symbols they
   *                    should be owned by `sym`
   *  @param body       the template statements without primary constructor
   *                    and value parameter fields.
   */
  def ClassDef(sym: Symbol, constrMods: Modifiers, vparamss: List[List[ValDef]], body: List[Tree], superPos: Position): ClassDef = {
    // "if they have symbols they should be owned by `sym`"
    assert(mforall(vparamss)(_.symbol.owner == sym), (mmap(vparamss)(_.symbol), sym))

    ClassDef(sym,
      gen.mkTemplate(sym.info.parents map TypeTree,
                    if (sym.thisSym == sym || phase.erasedTypes) noSelfType else ValDef(sym.thisSym),
                    constrMods, vparamss, body, superPos))
  }

 // --- subcomponents --------------------------------------------------

  object treeInfo extends {
    val global: Trees.this.type = self
  } with TreeInfo

  // --- additional cases in operations ----------------------------------

  override protected def xtraverse(traverser: Traverser, tree: Tree): Unit = tree match {
    case Parens(ts) =>
      traverser.traverseTrees(ts)
    case DocDef(comment, definition) =>
      traverser.traverse(definition)
    case SelectFromArray(qualifier, selector, erasure) =>
      traverser.traverse(qualifier)
    case InjectDerivedValue(arg) =>
      traverser.traverse(arg)
    case TypeTreeWithDeferredRefCheck() =>
      // (and rewrap the result? how to update the deferred check? would need to store wrapped tree instead of returning it from check)
    case _ => super.xtraverse(traverser, tree)
  }

  trait TreeCopier extends super.InternalTreeCopierOps {
    def DocDef(tree: Tree, comment: DocComment, definition: Tree): DocDef
    def SelectFromArray(tree: Tree, qualifier: Tree, selector: Name, erasure: Type): SelectFromArray
    def InjectDerivedValue(tree: Tree, arg: Tree): InjectDerivedValue
    def TypeTreeWithDeferredRefCheck(tree: Tree): TypeTreeWithDeferredRefCheck
  }

  def newStrictTreeCopier: TreeCopier = new StrictTreeCopier
  def newLazyTreeCopier: TreeCopier = new LazyTreeCopier

  class StrictTreeCopier extends super.StrictTreeCopier with TreeCopier {
    def DocDef(tree: Tree, comment: DocComment, definition: Tree) =
      new DocDef(comment, definition).copyAttrs(tree)
    def SelectFromArray(tree: Tree, qualifier: Tree, selector: Name, erasure: Type) =
      new SelectFromArray(qualifier, selector, erasure).copyAttrs(tree)
    def InjectDerivedValue(tree: Tree, arg: Tree) =
      new InjectDerivedValue(arg).copyAttrs(tree)
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
    def SelectFromArray(tree: Tree, qualifier: Tree, selector: Name, erasure: Type) = tree match {
      case t @ SelectFromArray(qualifier0, selector0, _)
      if (qualifier0 == qualifier) && (selector0 == selector) => t
      case _ => this.treeCopy.SelectFromArray(tree, qualifier, selector, erasure)
    }
    def InjectDerivedValue(tree: Tree, arg: Tree) = tree match {
      case t @ InjectDerivedValue(arg0)
      if (arg0 == arg) => t
      case _ => this.treeCopy.InjectDerivedValue(tree, arg)
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
          log(supplementErrorMessage("unhandled exception while transforming "+unit))
          throw ex
      }
    }
  }

  // used when a phase is disabled
  object noopTransformer extends Transformer {
    override def transformUnit(unit: CompilationUnit): Unit = {}
  }

  override protected def xtransform(transformer: super.Transformer, tree: Tree): Tree = tree match {
    case DocDef(comment, definition) =>
      transformer.treeCopy.DocDef(tree, comment, transformer.transform(definition))
    case SelectFromArray(qualifier, selector, erasure) =>
      transformer.treeCopy.SelectFromArray(
        tree, transformer.transform(qualifier), selector, erasure)
    case InjectDerivedValue(arg) =>
      transformer.treeCopy.InjectDerivedValue(
        tree, transformer.transform(arg))
    case TypeTreeWithDeferredRefCheck() =>
      transformer.treeCopy.TypeTreeWithDeferredRefCheck(tree)
  }

  object resetPos extends Traverser {
    override def traverse(t: Tree) {
      if (t != EmptyTree) t.setPos(NoPosition)
      super.traverse(t)
    }
  }

  /** resets symbol and tpe fields in a tree, @see ResetAttrs
   */
//  def resetAllAttrs[A<:Tree](x:A): A = { new ResetAttrsTraverser().traverse(x); x }
//  def resetLocalAttrs[A<:Tree](x:A): A = { new ResetLocalAttrsTraverser().traverse(x); x }

  def resetAllAttrs(x: Tree, leaveAlone: Tree => Boolean = null): Tree = new ResetAttrs(false, leaveAlone).transform(x)
  def resetLocalAttrs(x: Tree, leaveAlone: Tree => Boolean = null): Tree = new ResetAttrs(true, leaveAlone).transform(x)

  /** A transformer which resets symbol and tpe fields of all nodes in a given tree,
   *  with special treatment of:
   *    TypeTree nodes: are replaced by their original if it exists, otherwise tpe field is reset
   *                    to empty if it started out empty or refers to local symbols (which are erased).
   *    TypeApply nodes: are deleted if type arguments end up reverted to empty
   *    This(pkg) nodes where pkg is a package: these are kept.
   *
   *  (bq:) This transformer has mutable state and should be discarded after use
   */
  private class ResetAttrs(localOnly: Boolean, leaveAlone: Tree => Boolean = null, keepLabels: Boolean = false) {
    val debug = settings.debug.value
    val trace = scala.tools.nsc.util.trace when debug

    val locals = util.HashSet[Symbol](8)
    val orderedLocals = scala.collection.mutable.ListBuffer[Symbol]()
    def registerLocal(sym: Symbol) {
      if (sym != null && sym != NoSymbol) {
        if (debug && !(locals contains sym)) orderedLocals append sym
        locals addEntry sym
      }
    }

    class MarkLocals extends self.Traverser {
      def markLocal(tree: Tree) {
        if (tree.symbol != null && tree.symbol != NoSymbol) {
          val sym = tree.symbol
          registerLocal(sym)
          registerLocal(sym.sourceModule)
          registerLocal(sym.moduleClass)
          registerLocal(sym.companionClass)
          registerLocal(sym.companionModule)
          sym match {
            case sym: TermSymbol => registerLocal(sym.referenced)
            case _ => ;
          }
        }
      }

      override def traverse(tree: Tree) = {
        tree match {
         case _: DefTree | Function(_, _) | Template(_, _, _) =>
           markLocal(tree)
         case _ =>
           tree
        }

        super.traverse(tree)
      }
    }

    class Transformer extends self.Transformer {
      override def transform(tree: Tree): Tree = {
        if (leaveAlone != null && leaveAlone(tree))
          tree
        else
          super.transform {
            tree match {
              case tree if !tree.canHaveAttrs =>
                tree
              case tpt: TypeTree =>
                if (tpt.original != null)
                  transform(tpt.original)
                else {
                  val refersToLocalSymbols = tpt.tpe != null && (tpt.tpe exists (tp => locals contains tp.typeSymbol))
                  val isInferred = tpt.wasEmpty
                  if (refersToLocalSymbols || isInferred) {
                    tpt.duplicate.clearType()
                  } else {
                    tpt
                  }
                }
              // If one of the type arguments of a TypeApply gets reset to an empty TypeTree, then this means that:
              // 1) It isn't empty now (tpt.tpe != null), but it was empty before (tpt.wasEmpty).
              // 2) Thus, its argument got inferred during a preceding typecheck.
              // 3) Thus, all its arguments were inferred (because scalac can only infer all or nothing).
              // Therefore, we can safely erase the TypeApply altogether and have it inferred once again in a subsequent typecheck.
              // UPD: Actually there's another reason for erasing a type behind the TypeTree
              // is when this type refers to symbols defined in the tree being processed.
              // These symbols will be erased, because we can't leave alive a type referring to them.
              // Here we can only hope that everything will work fine afterwards.
              case TypeApply(fn, args) if args map transform exists (_.isEmpty) =>
                transform(fn)
              case EmptyTree =>
                tree
              case _ =>
                val dupl = tree.duplicate
                // Typically the resetAttrs transformer cleans both symbols and types.
                // However there are exceptions when we cannot erase symbols due to idiosyncrasies of the typer.
                // vetoXXX local variables declared below describe the conditions under which we cannot erase symbols.
                //
                // The first reason to not erase symbols is the threat of non-idempotency (SI-5464).
                // Here we take care of labels (SI-5562) and references to package classes (SI-5705).
                // There are other non-idempotencies, but they are not worked around yet.
                //
                // The second reason has to do with the fact that resetAttrs itself has limited usefulness.
                //
                // First of all, why do we need resetAttrs? Gor one, it's absolutely required to move trees around.
                // One cannot just take a typed tree from one lexical context and transplant it somewhere else.
                // Most likely symbols defined by those trees will become borked and the compiler will blow up (SI-5797).
                // To work around we just erase all symbols and types and then hope that we'll be able to correctly retypecheck.
                // For ones who're not affected by scalac Stockholm syndrome, this might seem to be an extremely naive fix, but well...
                //
                // Of course, sometimes erasing everything won't work, because if a given identifier got resolved to something
                // in one lexical scope, it can get resolved to something else.
                //
                // What do we do in these cases? Enter the workaround for the workaround: resetLocalAttrs, which only destroys
                // locally defined symbols, but doesn't touch references to stuff declared outside of a given tree.
                // That's what localOnly and vetoScope are for.
                if (dupl.hasSymbol) {
                  val sym = dupl.symbol
                  val vetoScope = localOnly && !(locals contains sym)
                  val vetoLabel = keepLabels && sym.isLabel
                  val vetoThis = dupl.isInstanceOf[This] && sym.isPackageClass
                  if (!(vetoScope || vetoLabel || vetoThis)) dupl.symbol = NoSymbol
                }
                dupl.clearType()
            }
          }
      }
    }

    def transform(x: Tree): Tree = {
      if (localOnly)
      new MarkLocals().traverse(x)

      if (localOnly && debug) {
        assert(locals.size == orderedLocals.size)
        val msg = orderedLocals.toList filter {_ != NoSymbol} map {"  " + _} mkString EOL
        trace("locals (%d total): %n".format(orderedLocals.size))(msg)
      }

      new Transformer().transform(x)
    }
  }

  /* New pattern matching cases:

   case Parens(expr)                                               (only used during parsing)
   case DocDef(comment, defn) =>                                   (eliminated by typer)
   case TypeTreeWithDeferredRefCheck() =>                          (created and eliminated by typer)
   case SelectFromArray(_, _, _) =>                                (created and eliminated by erasure)
   case InjectDerivedValue(_) =>                                   (created and eliminated by erasure)

  */

 }
