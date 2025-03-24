/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package typechecker

trait StdAttachments {
  self: Analyzer =>

  import global._

  /** Carries information necessary to expand the host tree.
   *  At times we need to store this info, because macro expansion can be delayed until its targs are inferred.
   *  After a macro application has been successfully expanded, this attachment is destroyed.
   */
  type UnaffiliatedMacroContext = scala.reflect.macros.contexts.Context
  type MacroContext = UnaffiliatedMacroContext { val universe: self.global.type }
  case class MacroRuntimeAttachment(delayed: Boolean, typerContext: Context, macroContext: Option[MacroContext])

  /** Scratchpad for the macro expander, which is used to store all intermediate data except the details about the runtime.
   */
  case class MacroExpanderAttachment(original: Tree, desugared: Tree)

  /** Loads underlying MacroExpanderAttachment from a macro expandee or returns a default value for that attachment.
   */
  def macroExpanderAttachment(tree: Tree): MacroExpanderAttachment =
    tree.attachments.get[MacroExpanderAttachment] getOrElse {
      tree match {
        case Apply(fn, _) if tree.isInstanceOf[ApplyToImplicitArgs] => macroExpanderAttachment(fn)
        case _ => MacroExpanderAttachment(tree, EmptyTree)
      }
    }

  /** After macro expansion is completed, links the expandee and the expansion result
   *  by annotating them both with a `MacroExpansionAttachment`.
   */
  def linkExpandeeAndDesugared(expandee: Tree, desugared: Tree): Unit = {
    val metadata = MacroExpanderAttachment(expandee, desugared)
    expandee updateAttachment metadata
    desugared updateAttachment metadata
  }

  /** Is added by the macro engine to originals and results of macro expansions.
   *  Stores the original expandee as it entered the `macroExpand` function.
   */
  case class MacroExpansionAttachment(expandee: Tree, expanded: Any)

  /** Determines whether the target is either an original or a result of a macro expansion.
   *  The parameter is of type `Any`, because macros can expand both into trees and into annotations.
   */
  def hasMacroExpansionAttachment(any: Any): Boolean = any match {
    case tree: Tree => tree.hasAttachment[MacroExpansionAttachment]
    case _ => false
  }

  /** Returns the original tree of the macro expansion if the argument is a macro expansion or EmptyTree otherwise.
   */
  def macroExpandee(tree: Tree): Tree = tree.attachments.get[MacroExpansionAttachment].map(_.expandee).getOrElse(EmptyTree)

  /** After macro expansion is completed, links the expandee and the expansion result by annotating them both with a `MacroExpansionAttachment`.
   *  The `expanded` parameter is of type `Any`, because macros can expand both into trees and into annotations.
   */
  def linkExpandeeAndExpanded(expandee: Tree, expanded: Any): Unit = {
    val metadata = MacroExpansionAttachment(expandee, expanded)
    expandee updateAttachment metadata
    expanded match {
      case expanded: Tree if !expanded.isEmpty => expanded updateAttachment metadata
      case _ => // do nothing
    }
  }

  /** When present, suppresses macro expansion for the host.
   *  This is occasionally necessary, e.g. to prohibit eta-expansion of macros.
   *
   *  Does not affect expandability of child nodes, there's context.withMacrosDisabled for that
   *  (but think thrice before using that API - see the discussion at https://github.com/scala/scala/pull/1639).
   */
  case object SuppressMacroExpansionAttachment

  /** Suppresses macro expansion of the tree by putting SuppressMacroExpansionAttachment on it.
   */
  def suppressMacroExpansion(tree: Tree): tree.type = tree.updateAttachment(SuppressMacroExpansionAttachment)

  /** Unsuppresses macro expansion of the tree by removing SuppressMacroExpansionAttachment from it and its children.
   */
  def unsuppressMacroExpansion(tree: Tree): tree.type = {
    tree.removeAttachment[SuppressMacroExpansionAttachment.type]
    tree match {
      // see the comment to `isMacroExpansionSuppressed` to learn why we need
      // a special traversal strategy here
      case Apply(fn, _) => unsuppressMacroExpansion(fn)
      case TypeApply(fn, _) => unsuppressMacroExpansion(fn)
      case _ => // do nothing
    }
    tree
  }

  /** Determines whether a tree should not be expanded, because someone has put SuppressMacroExpansionAttachment on it or one of its children.
   */
  def isMacroExpansionSuppressed(tree: Tree): Boolean =
    (  settings.Ymacroexpand.value == settings.MacroExpand.None // scala/bug#6812
    || tree.hasAttachment[SuppressMacroExpansionAttachment.type]
    || (tree match {
        // we have to account for the fact that during typechecking an expandee might become wrapped,
        // i.e. surrounded by an inferred implicit argument application or by an inferred type argument application.
        // in that case the expandee itself will no longer be suppressed and we need to look at the core
        case Apply(fn, _)     => isMacroExpansionSuppressed(fn)
        case TypeApply(fn, _) => isMacroExpansionSuppressed(fn)
        case _                => false
      })
    )

  /** After being synthesized by the parser, primary constructors aren't fully baked yet.
   *  A call to super in such constructors is just a fill-me-in-later dummy resolved later
   *  by `parentTypes`. This attachment coordinates `parentTypes` and `typedTemplate` and
   *  allows them to complete the synthesis.
   */
  case class SuperArgsAttachment(argss: List[List[Tree]])

  /** Convenience method for `SuperArgsAttachment`.
   *  Compared with `MacroRuntimeAttachment` this attachment has different a usage pattern,
   *  so it really benefits from a dedicated extractor.
   */
  def superArgs(tree: Tree): Option[List[List[Tree]]] =
    tree.attachments.get[SuperArgsAttachment] collect { case SuperArgsAttachment(argss) => argss }

  /** Determines whether the given tree has an associated SuperArgsAttachment.
   */
  def hasSuperArgs(tree: Tree): Boolean = superArgs(tree).nonEmpty

  /** @see markMacroImplRef
   */
  case object MacroImplRefAttachment

  /** Marks the tree as a macro impl reference, which is a naked reference to a method.
   *
   *  This is necessary for typechecking macro impl references (see `DefaultMacroCompiler.defaultResolveMacroImpl`),
   *  because otherwise typing a naked reference will result in the "follow this method with `_` if you want to
   *  treat it as a partially applied function" errors.
   *
   *  This mark suppresses adapt except for when the annottee is a macro application.
   */
  def markMacroImplRef(tree: Tree): Tree = tree.updateAttachment(MacroImplRefAttachment)

  /** Unmarks the tree as a macro impl reference (see `markMacroImplRef` for more information).
   *
   *  This is necessary when a tree that was previously deemed to be a macro impl reference,
   *  typechecks to be a macro application. Then we need to unmark it, expand it and try to treat
   *  its expansion as a macro impl reference.
   */
  def unmarkMacroImplRef(tree: Tree): Tree = tree.removeAttachment[MacroImplRefAttachment.type](MacroImplRefAttachmentTag)

  /** Determines whether a tree should or should not be adapted,
   *  because someone has put MacroImplRefAttachment on it.
   */
  def isMacroImplRef(tree: Tree): Boolean = tree.hasAttachment[MacroImplRefAttachment.type](MacroImplRefAttachmentTag)
  private[this] val MacroImplRefAttachmentTag: reflect.ClassTag[MacroImplRefAttachment.type] = reflect.classTag[MacroImplRefAttachment.type]

  /** Since mkInvoke, the applyDynamic/selectDynamic/etc desugarer, is disconnected
   *  from typedNamedApply, the applyDynamicNamed argument rewriter, the latter
   *  doesn’t know whether it needs to apply the rewriting because the application
   *  has just been desugared or it needs to hold on because it’s already performed
   *  a desugaring on this tree. This has led to scala/bug#8006.
   *
   *  This attachment solves the problem by providing a means of communication
   *  between the two Dynamic desugarers, which solves the aforementioned issue.
   */
  case object DynamicRewriteAttachment
  def markDynamicRewrite(tree: Tree): Tree = tree.updateAttachment(DynamicRewriteAttachment)
  def unmarkDynamicRewrite(tree: Tree): Tree = tree.removeAttachment[DynamicRewriteAttachment.type](DynamicRewriteAttachmentTag)
  def isDynamicRewrite(tree: Tree): Boolean = tree.attachments.get[DynamicRewriteAttachment.type](DynamicRewriteAttachmentTag).isDefined
  private[this] val DynamicRewriteAttachmentTag: reflect.ClassTag[DynamicRewriteAttachment.type] = reflect.classTag[DynamicRewriteAttachment.type]

  /**
   * Marks a tree that has been adapted by typer and sets the original tree that was in place before.
   * 
   * Keeping track of the original trees were is an important feature for some compiler plugins (like
   * Scalameta) and the incremental compiler (Zinc). In both cases, adapting trees loses information
   * in some sense and do not allow external tools to capture some information stored in user-defined
   * trees that are optimized away by early phases (mostly, typer).
   * 
   * See how the absence of this attachment blocks Zinc: https://github.com/sbt/zinc/issues/227.
   * Related: https://github.com/scala/scala-dev/issues/340.
   * 
   * This attachment is, at the moment, only used to keep track of constant-folded constants. It
   * has a generic wording in the hope that in the future can be reused in the same context to keep
   * track of other adapted trees.
   */
  case class OriginalTreeAttachment(original: Tree)
}


// imported from scalamacros/paradise
trait MacroAnnotationAttachments {
  self: Analyzer =>

  import global._
  import scala.collection.mutable

  case object WeakSymbolAttachment
  def markWeak(sym: Symbol) = if (sym != null && sym != NoSymbol) sym.updateAttachment(WeakSymbolAttachment) else sym
  def unmarkWeak(sym: Symbol) = if (sym != null && sym != NoSymbol) sym.removeAttachment[WeakSymbolAttachment.type] else sym
  def isWeak(sym: Symbol) = sym == null || sym == NoSymbol || sym.attachments.get[WeakSymbolAttachment.type].isDefined

  case class SymbolCompleterAttachment(info: Type)
  def backupCompleter(sym: Symbol): Symbol = {
    if (sym != null && sym != NoSymbol) {
      assert(sym.rawInfo.isInstanceOf[LazyType], s"${sym.accurateKindString} ${sym.rawname}#${sym.id} with ${sym.rawInfo.kind}")
      sym.updateAttachment(SymbolCompleterAttachment(sym.rawInfo))
    } else sym
  }
  def restoreCompleter(sym: Symbol): Unit = {
    if (sym != null && sym != NoSymbol) {
      val oldCompleter = sym.attachments.get[SymbolCompleterAttachment].get.info
      sym setInfo oldCompleter
      sym.attachments.remove[SymbolCompleterAttachment]
    } else ()
  }

  // here we should really store and retrieve duplicates of trees in order to avoid leakage through tree attributes
  case class SymbolSourceAttachment(source: Tree)
  def attachSource(sym: Symbol, tree: Tree): Symbol = if (sym != null && sym != NoSymbol) sym.updateAttachment(SymbolSourceAttachment(duplicateAndKeepPositions(tree))) else sym
  def attachedSource(sym: Symbol): Tree = if (sym != null && sym != NoSymbol) sym.attachments.get[SymbolSourceAttachment].map(att => duplicateAndKeepPositions(att.source)).getOrElse(EmptyTree) else EmptyTree

  // unfortunately we cannot duplicate here, because that would dissociate the symbol from its derived symbols
  // that's because attachExpansion(tree) happens prior to enterSym(tree), so if we duplicate the assigned symbol never makes it into the att
  // in its turn, that would mean that we won't be able to handle recursive expansions in typedTemplate
  // because by the time typedTemplate gets activated, everything's already expanded by templateSig
  // so we need to go from original trees/symbols to recursively expanded ones and that requires links to derived symbols
  // TODO: should be a better solution
  case class SymbolExpansionAttachment(expansion: List[Tree])
  def hasAttachedExpansion(sym: Symbol) = sym.attachments.get[SymbolExpansionAttachment].isDefined
  def attachExpansion(sym: Symbol, trees: List[Tree]): Symbol = if (sym != null && sym != NoSymbol) sym.updateAttachment(SymbolExpansionAttachment(trees/*.map(tree => duplicateAndKeepPositions(tree))*/)) else sym
  def attachedExpansion(sym: Symbol): Option[List[Tree]] = if (sym != null && sym != NoSymbol) sym.attachments.get[SymbolExpansionAttachment].map(_.expansion/*.map(tree => duplicateAndKeepPositions(tree))*/) else None

  import SymbolExpansionStatus._
  private def checkExpansionStatus(sym: Symbol, p: SymbolExpansionStatus => Boolean) = sym.attachments.get[SymbolExpansionStatus].map(p).getOrElse(false)
  def isMaybeExpandee(sym: Symbol): Boolean = checkExpansionStatus(sym, _.isUnknown)
  def isExpanded(sym: Symbol): Boolean = checkExpansionStatus(sym, _.isExpanded)
  def isNotExpandable(sym: Symbol): Boolean = checkExpansionStatus(sym, _.isNotExpandable)
  def markMaybeExpandee(sym: Symbol): Symbol = if (sym != null && sym != NoSymbol) sym.updateAttachment(Unknown) else sym
  def markExpanded(sym: Symbol): Symbol = if (sym != null && sym != NoSymbol) sym.updateAttachment(Expanded) else sym
  def markNotExpandable(sym: Symbol): Symbol = if (sym != null && sym != NoSymbol) sym.updateAttachment(NotExpandable) else sym
  def unmarkExpanded(sym: Symbol): Symbol = if (sym != null && sym != NoSymbol) sym.removeAttachment[SymbolExpansionStatus] else sym

  case class CacheAttachment(cache: mutable.Map[String, Any])
  implicit class RichTree(tree: Tree) {
    def cached[T](key: String, op: => T): T = {
      val cache = tree.attachments.get[CacheAttachment].map(_.cache).getOrElse(mutable.Map[String, Any]())
      val result = cache.getOrElseUpdate(key, op).asInstanceOf[T]
      tree.updateAttachment(CacheAttachment(cache))
      result
    }
  }

  private final class SymbolExpansionStatus private (val value: Int) { //extends AnyVal {
    def isUnknown = this == SymbolExpansionStatus.Unknown
    def isExpanded = this == SymbolExpansionStatus.Expanded
    def isNotExpandable = this == SymbolExpansionStatus.NotExpandable
  }
  private object SymbolExpansionStatus {
    val Unknown = new SymbolExpansionStatus(0)
    val Expanded = new SymbolExpansionStatus(1)
    val NotExpandable = new SymbolExpansionStatus(2)
  }
}
