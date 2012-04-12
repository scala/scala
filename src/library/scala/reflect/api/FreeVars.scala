package scala.reflect
package api

trait FreeVars {
  self: Universe =>

  /** Represents a free term captured by reification.
   */
  type FreeTerm <: Symbol

  val FreeTerm: FreeTermExtractor

  abstract class FreeTermExtractor {
    def unapply(freeTerm: FreeTerm): Option[(TermName, Type, Any, String)]
  }

  /** Extracts free terms from a tree that is reified or contains reified subtrees.
   */
  def freeTerms(tree: Tree): List[FreeTerm]

  /** Represents a free type captured by reification.
   */
  type FreeType <: Symbol

  val FreeType: FreeTypeExtractor

  abstract class FreeTypeExtractor {
    def unapply(freeType: FreeType): Option[(TypeName, Type, String)]
  }

  /** Extracts free types from a tree that is reified or contains reified subtrees.
   */
  def freeTypes(tree: Tree): List[FreeType]

  /** Substitutes free types in a reified tree.
   */
  def substituteFreeTypes(tree: Tree, subs: Map[FreeType, Type]): Tree

  /** Substitutes free types in a reified type.
   */
  def substituteFreeTypes(tpe: Type, subs: Map[FreeType, Type]): Type
}
