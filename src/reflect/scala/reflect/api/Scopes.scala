package scala.reflect
package api

/**
 * Defines the type hierachy for scopes.
 *
 * @see [[scala.reflect]] for a description on how the class hierarchy is encoded here.
 */
trait Scopes { self: Universe =>

  /** The base type of all scopes. A scope object generally maps names to symbols available in the current lexical scope.
   *  Scopes can be nested. This base type, however, only exposes a minimal interface, representing a scope as an iterable of symbols.
   */
  type Scope >: Null <: ScopeApi

  /** The API that all scopes support */
  trait ScopeApi extends Iterable[Symbol]

  /** A tag that preserves the identity of the `Scope` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val ScopeTag: ClassTag[Scope]

  /** Create a new scope with the given initial elements. */
  def newScopeWith(elems: Symbol*): Scope

  /** The type of member scopes, as in class definitions, for example. */
  type MemberScope >: Null <: Scope with MemberScopeApi

  /** The API that all member scopes support */
  trait MemberScopeApi extends ScopeApi {
    /** Sorts the symbols included in this scope so that:
     *    1) Symbols appear in the linearization order of their owners.
     *    2) Symbols with the same owner appear in same order of their declarations.
     *    3) Synthetic members (e.g. getters/setters for vals/vars) might appear in arbitrary order.
     */
    def sorted: List[Symbol]
  }

  /** A tag that preserves the identity of the `MemberScope` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val MemberScopeTag: ClassTag[MemberScope]
}