package scala.tools.nsc
package typechecker

trait StdAttachments {
  self: Analyzer =>

  import global._

  /** Carries information necessary to expand the host tree.
   *  At times we need to store this info, because macro expansion can be delayed until its targs are inferred.
   *  After a macro application has been successfully expanded, this attachment is destroyed.
   */
  type UnaffiliatedMacroContext = scala.reflect.macros.runtime.Context
  type MacroContext = UnaffiliatedMacroContext { val universe: self.global.type }
  case class MacroRuntimeAttachment(delayed: Boolean, typerContext: Context, macroContext: Option[MacroContext])

  /** Scratchpad for the macro expander, which is used to store all intermediate data except the details about the runtime.
   */
  case class MacroExpanderAttachment(original: Tree, desugared: Tree, role: MacroRole, enclosingTemplate: Option[Template])

  /** Loads underlying MacroExpanderAttachment from a macro expandee or returns a default value for that attachment.
   */
 def macroExpanderAttachment(tree: Tree): MacroExpanderAttachment =
    tree.attachments.get[MacroExpanderAttachment] getOrElse {
      tree match {
        case Apply(fn, _) if tree.isInstanceOf[ApplyToImplicitArgs] => macroExpanderAttachment(fn)
        case _ => MacroExpanderAttachment(tree, EmptyTree, APPLY_ROLE, None)
      }
    }

  /** After macro expansion is completed, links the expandee and the expansion result
   *  by annotating them both with a `MacroExpansionAttachment`.
   */
  def linkExpandeeAndDesugared(expandee: Tree, desugared: Tree, role: MacroRole, template: Option[Template]): Unit = {
    val metadata = MacroExpanderAttachment(expandee, desugared, role, template)
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
    case tree: Tree => tree.attachments.get[MacroExpansionAttachment].isDefined
    case _ => false
  }

  /** After macro expansion is completed, links the expandee and the expansion result by annotating them both with a `MacroExpansionAttachment`.
   *  The `expanded` parameter is of type `Any`, because macros can expand both into trees and into annotations.
   */
  def linkExpandeeAndExpanded(expandee: Tree, expanded: Any): Unit = {
    val metadata = MacroExpansionAttachment(expandee, expanded)
    expandee updateAttachment metadata
    expanded match {
      case expanded: Tree => expanded updateAttachment metadata
      case _ => // do nothing
    }
  }

  /** Checks whether there is a Template resulting from a macro expansion and associated with the current tree.
   *  Such templates indicate a type macro in parent role that has been expanded into a template.
   */
  object ExpandedIntoTemplate {
    def unapply(tree: Tree): Option[Template] = tree.attachments.get[MacroExpansionAttachment] match {
      case Some(MacroExpansionAttachment(_, template: Template)) => Some(template)
      case _ => None
    }
  }

  /** Checks whether there is any tree resulting from a macro expansion and associated with the current tree.
   */
  object ExpandedIntoTree {
    def unapply(tree: Tree): Option[Tree] = tree.attachments.get[MacroExpansionAttachment] match {
      case Some(MacroExpansionAttachment(_, tree: Tree)) => Some(tree)
      case _ => None
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
      case Apply(fn, _) if tree.isInstanceOf[ApplyToImplicitArgs] => unsuppressMacroExpansion(fn)
      case _ => // do nothing
    }
    tree
  }

  /** Determines whether a tree should not be expanded, because someone has put SuppressMacroExpansionAttachment on it or one of its children.
   */
  def isMacroExpansionSuppressed(tree: Tree): Boolean =
    if (tree.attachments.get[SuppressMacroExpansionAttachment.type].isDefined) true
    else tree match {
      case Apply(fn, _) if tree.isInstanceOf[ApplyToImplicitArgs] => isMacroExpansionSuppressed(fn)
      case _ => false
    }

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
}