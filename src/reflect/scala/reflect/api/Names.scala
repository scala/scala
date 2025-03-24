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

package scala
package reflect
package api

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 * This trait defines `Name`s in Scala Reflection, and operations on them.
 *
 *  Names are simple wrappers for strings. [[scala.reflect.api.Names#Name Name]] has two subtypes
 *  [[scala.reflect.api.Names#TermName TermName]] and [[scala.reflect.api.Names#TypeName TypeName]]
 *  which distinguish names of terms (like objects or members) and types. A term and a type of the
 *  same name can co-exist in an object.
 *
 *  To search for the `map` method (which is a term) declared in the `List` class, one can do:
 *
 * {{{
 *   scala> typeOf[List[_]].member(TermName("map"))
 *   res0: reflect.runtime.universe.Symbol = method map
 * }}}
 *
 *  To search for a type member, one can follow the same procedure, using `TypeName` instead.
 *
 *  For more information about creating and using `Name`s, see the [[https://docs.scala-lang.org/overviews/reflection/annotations-names-scopes.html Reflection Guide: Annotations, Names, Scopes, and More]]
 *
 *  @contentDiagram hideNodes "*Api"
 *  @group ReflectionAPI
 */
trait Names {
  /** A former implicit conversion from String to TermName.
   *
   *  This used to be an implicit conversion, enabling an alternative notation
   *  `"map": TermName` as opposed to `TermName("map")`. It is only kept for
   *  binary compatibility reasons, and should not be used anymore.
   *
   *  @group Names
   */
  @deprecated("use `TermName(s)` instead", "2.11.0")
  def stringToTermName(s: String): TermName = TermName(s)

  /** A former implicit conversion from String to TypeName.
   *
   *  This used to be an implicit conversion, enabling an alternative notation
   *  `"List": TypeName` as opposed to `TypeName("List")`. It is only kept for
   *  binary compatibility reasons, and should not be used anymore.
   *
   *  @group Names
   */
  @deprecated("use `TypeName(s)` instead", "2.11.0")
  def stringToTypeName(s: String): TypeName = TypeName(s)

  /** The abstract type of names.
   *  @group Names
   */
  type Name >: Null <: AnyRef with NameApi

  /** The abstract type of names representing terms.
   *  @group Names
   */
  type TypeName >: Null <: TypeNameApi with Name

  /** Has no special methods. Is here to provides erased identity for `TypeName`.
   *  @group API
   */
  trait TypeNameApi

  /** The abstract type of names representing types.
   *  @group Names
   */
  type TermName >: Null <: TermNameApi with Name

  /** Has no special methods. Is here to provides erased identity for `TermName`.
   *  @group API
   */
  trait TermNameApi

  /** The API of Name instances.
   *  @group API
   */
  abstract class NameApi {
    /** Checks whether the name is a term name */
    def isTermName: Boolean

    /** Checks whether the name is a type name */
    def isTypeName: Boolean

    /** Returns a term name that wraps the same string as `this` */
    def toTermName: TermName

    /** Returns a type name that wraps the same string as `this` */
    def toTypeName: TypeName

    /** Replaces all occurrences of \$op_names in this name by corresponding operator symbols.
     *  Example: `foo_\$plus\$eq` becomes `foo_+=`
     */
    @deprecated("use `decodedName.toString` instead", "2.11.0")
    def decoded: String

    /** Replaces all occurrences of operator symbols in this name by corresponding \$op_names.
     *  Example: `foo_+=` becomes `foo_\$plus\$eq`.
     */
    @deprecated("use `encodedName.toString` instead", "2.11.0")
    def encoded: String

    /** The decoded name, still represented as a name.
     */
    def decodedName: Name

    /** The encoded name, still represented as a name.
     */
    def encodedName: Name
  }

  /** Create a new term name.
   *  @group Names
   */
  @deprecated("use TermName instead", "2.11.0")
  def newTermName(s: String): TermName

  /** Creates a new type name.
   *  @group Names
   */
  @deprecated("use TypeName instead", "2.11.0")
  def newTypeName(s: String): TypeName

  /** The constructor/extractor for `TermName` instances.
   *  @group Extractors
   */
  val TermName: TermNameExtractor

  /** An extractor class to create and pattern match with syntax `TermName(s)`.
   *  @group Extractors
   */
  abstract class TermNameExtractor {
    def apply(s: String): TermName
    def unapply(name: TermName): Option[String]
  }

  /** The constructor/extractor for `TypeName` instances.
   *  @group Extractors
   */
  val TypeName: TypeNameExtractor

  /** An extractor class to create and pattern match with syntax `TypeName(s)`.
   *  @group Extractors
   */
  abstract class TypeNameExtractor {
    def apply(s: String): TypeName
    def unapply(name: TypeName): Option[String]
  }
}
