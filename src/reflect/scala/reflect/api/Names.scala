package scala.reflect
package api

/** A trait that manages names.
 *  A name is a string in one of two name universes: terms and types.
 *  The same string can be a name in both universes.
 *  Two names are equal if they represent the same string and they are
 *  members of the same universe.
 *
 *  Names are interned. That is, for two names `name11 and `name2`,
 *  `name1 == name2` implies `name1 eq name2`.
 */
trait Names extends base.Names {

  /** The abstract type of names */
  type Name >: Null <: NameApi

  /** The extended API of names that's supported on reflect mirror via an
   *  implicit conversion in reflect.ops
   */
  abstract class NameApi extends NameBase {

    // [Eugene++] this functionality should be in base
    // this is because stuff will be reified in mangled state, and people will need a way to figure it out

    /** Replaces all occurrences of \$op_names in this name by corresponding operator symbols.
     *  Example: `foo_\$plus\$eq` becomes `foo_+=`
     */
    def decoded: String

    /** Replaces all occurrences of operator symbols in this name by corresponding \$op_names.
     *  Example: `foo_+=` becomes `foo_\$plus\$eq`.
     */
    def encoded: String

    /** The decoded name, still represented as a name.
     */
    def decodedName: Name

    /** The encoded name, still represented as a name.
     */
    def encodedName: Name
  }
}
