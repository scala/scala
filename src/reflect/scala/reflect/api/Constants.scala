/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package api

trait Constants extends base.Constants {
  self: Universe =>

  override type Constant >: Null <: AnyRef with ConstantApi

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
