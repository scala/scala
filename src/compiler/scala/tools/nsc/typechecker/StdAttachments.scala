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
  def suppressMacroExpansion(tree: Tree) = tree.updateAttachment(SuppressMacroExpansionAttachment)

  /** Unsuppresses macro expansion of the tree by removing SuppressMacroExpansionAttachment from it and its children.
   */
  def unsuppressMacroExpansion(tree: Tree): Tree = {
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
    (  settings.Ymacroexpand.value == settings.MacroExpand.None // SI-6812
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
  def unmarkMacroImplRef(tree: Tree): Tree = tree.removeAttachment[MacroImplRefAttachment.type]

  /** Determines whether a tree should or should not be adapted,
   *  because someone has put MacroImplRefAttachment on it.
   */
  def isMacroImplRef(tree: Tree): Boolean = tree.hasAttachment[MacroImplRefAttachment.type]

  /** Since mkInvoke, the applyDynamic/selectDynamic/etc desugarer, is disconnected
   *  from typedNamedApply, the applyDynamicNamed argument rewriter, the latter
   *  doesn’t know whether it needs to apply the rewriting because the application
   *  has just been desugared or it needs to hold on because it’s already performed
   *  a desugaring on this tree. This has led to SI-8006.
   *
   *  This attachment solves the problem by providing a means of communication
   *  between the two Dynamic desugarers, which solves the aforementioned issue.
   */
  case object DynamicRewriteAttachment
  def markDynamicRewrite(tree: Tree): Tree = tree.updateAttachment(DynamicRewriteAttachment)
  def unmarkDynamicRewrite(tree: Tree): Tree = tree.removeAttachment[DynamicRewriteAttachment.type]
  def isDynamicRewrite(tree: Tree): Boolean = tree.attachments.get[DynamicRewriteAttachment.type].isDefined
}
