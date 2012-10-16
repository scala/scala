package scala.reflect
package api

/** This trait provides support for scopes in the reflection API.
 *
 *  A scope object generally maps names to symbols available in a corresponding lexical scope.
 *  Scopes can be nested. The base type exposed to the reflection API, however,
 *  only exposes a minimal interface, representing a scope as an iterable of symbols.
 *
 *  For rare occasions when it is necessary to create a scope manually,
 *  e.g., to populate members of [[scala.reflect.api.Types#RefinedType]],
 *  there is the `newScopeWith` function.
 *
 *  Additional functionality is exposed in member scopes that are returned by
 *  `members` and `declarations` defined in [[scala.reflect.api.Types#TypeApi]].
 *  Such scopes support the `sorted` method, which sorts members in declaration order.
 */
trait Scopes { self: Universe =>

  /** The base type of all scopes.
   *  @template
   *  @group Scopes
   */
  type Scope >: Null <: ScopeApi

  /** The API that all scopes support
   *  @group API
   */
  trait ScopeApi extends Iterable[Symbol]

  /** A tag that preserves the identity of the `Scope` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   *  @group Tags
   */
  implicit val ScopeTag: ClassTag[Scope]

  /** Create a new scope with the given initial elements.
   *  @group Scopes
   */
  def newScopeWith(elems: Symbol*): Scope

  /** The type of member scopes, as in class definitions, for example.
   *  @template
   *  @group Scopes
   */
  type MemberScope >: Null <: Scope with MemberScopeApi

  /** The API that all member scopes support
   *  @group API
   */
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
   *  @group Tags
   */
  implicit val MemberScopeTag: ClassTag[MemberScope]
}