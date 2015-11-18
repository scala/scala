/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package ast

import scala.reflect.ClassTag
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
  implicit val TreeCopierTag: ClassTag[TreeCopier] = ClassTag[TreeCopier](classOf[TreeCopier])

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

  // Finally, no one uses resetAllAttrs anymore, so I'm removing it from the compiler.
  // Even though it's with great pleasure I'm doing that, I'll leave its body here to warn future generations about what happened in the past.
  //
  // So what actually happened in the past is that we used to have two flavors of resetAttrs: resetAllAttrs and resetLocalAttrs.
  // resetAllAttrs destroyed all symbols and types in the tree in order to reset its state to something suitable for retypechecking
  // and/or embedding into bigger trees / different lexical scopes. (Btw here's some background on why people would want to use
  // reset attrs in the first place: https://groups.google.com/forum/#!topic/scala-internals/TtCTPlj_qcQ).
  //
  // However resetAllAttrs was more of a poison than of a treatment, because along with locally defined symbols that are the cause
  // for almost every or maybe even every case of tree corruption, it erased external bindings that sometimes could not be restored.
  // This is how we came up with resetLocalAttrs that left external bindings alone, and that was a big step forward.
  // Then slowly but steadily we've evicted all usages of resetAllAttrs from our codebase in favor of resetLocalAttrs
  // and have been living happily ever after.
  //
  // def resetAllAttrs(x: Tree, leaveAlone: Tree => Boolean = null): Tree = new ResetAttrs(localOnly = false, leaveAlone).transform(x)

  // upd. Unfortunately this didn't work out quite as we expected. The last two users of resetAllAttrs:
  // reification and typedLabelDef broke in very weird ways when we replaced resetAllAttrs with resetLocalAttrs
  // (see SI-8316 change from resetAllAttrs to resetLocalAttrs in reifiers broke Slick and
  // SI-8318 NPE in mixin in scala-continuations for more information).
  // Given that we're supposed to release 2.11.0-RC1 in less than a week, I'm temporarily reinstating resetAllAttrs
  // until we have time to better understand what's going on. In order to dissuade people from using it,
  // it now comes with a new, ridiculous name.
  /** @see ResetAttrs */
  def brutallyResetAttrs(x: Tree, leaveAlone: Tree => Boolean = null): Tree = new ResetAttrs(brutally = true, leaveAlone).transform(x)

  /** @see ResetAttrs */
  def resetAttrs(x: Tree): Tree = new ResetAttrs(brutally = false, leaveAlone = null).transform(x)

  /** A transformer which resets symbol and tpe fields of all nodes in a given tree,
   *  with special treatment of:
   *    TypeTree nodes: are replaced by their original if it exists, otherwise tpe field is reset
   *                    to empty if it started out empty or refers to local symbols (which are erased).
   *    TypeApply nodes: are deleted if type arguments end up reverted to empty
   *    This(pkg) nodes where pkg is a package: these are kept.
   *
   *  (bq:) This transformer has mutable state and should be discarded after use
   */
  private class ResetAttrs(brutally: Boolean, leaveAlone: Tree => Boolean) {
    // this used to be based on -Ydebug, but the need for logging in this code is so situational
    // that I've reverted to a hard-coded constant here.
    val debug = false
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
          registerLocal(sym.deSkolemize)
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
                // Here we take care of references to package classes (SI-5705).
                // There are other non-idempotencies, but they are not worked around yet.
                //
                // The second reason has to do with the fact that resetAttrs needs to be less destructive.
                // Erasing locally-defined symbols is useful to prevent tree corruption, but erasing external bindings is not,
                // therefore we want to retain those bindings, especially given that restoring them can be impossible
                // if we move these trees into lexical contexts different from their original locations.
                if (dupl.hasSymbolField) {
                  val sym = dupl.symbol
                  val vetoScope = !brutally && !(locals contains sym) && !(locals contains sym.deSkolemize)
                  val vetoThis = dupl.isInstanceOf[This] && sym.isPackageClass
                  if (!(vetoScope || vetoThis)) dupl.symbol = NoSymbol
                }
                dupl.clearType()
            }
          }
      }
    }

    def transform(x: Tree): Tree = {
      new MarkLocals().traverse(x)

      if (debug) {
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
