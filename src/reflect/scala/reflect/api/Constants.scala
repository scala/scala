/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package api

/**
 * Defines the type hierachy for compile-time constants.
 *
 * @see [[scala.reflect]] for a description on how the class hierarchy is encoded here.
 */
trait Constants {
  self: Universe =>

  /** The type of compile-time constants.
   */
  type Constant >: Null <: AnyRef with ConstantApi

  /** A tag that preserves the identity of the `Constant` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val ConstantTag: ClassTag[Constant]

  /** The constructor/deconstructor for `Constant` instances. */
  val Constant: ConstantExtractor

  /** An extractor class to create and pattern match with syntax `Constant(value)`
   *  where `value` is the Scala value of the constant.
   */
  abstract class ConstantExtractor {
    def apply(value: Any): Constant
    def unapply(arg: Constant): Option[Any]
  }

  abstract class ConstantApi {
    val value: Any
    def tpe: Type
    def isNaN: Boolean

    def booleanValue: Boolean
    def byteValue: Byte
    def shortValue: Short
    def charValue: Char
    def intValue: Int
    def longValue: Long
    def floatValue: Float
    def doubleValue: Double
    def stringValue: String
    def typeValue: Type
    def symbolValue: Symbol

    def convertTo(pt: Type): Constant
  }
}
