/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package api

trait StandardDefinitions { self: Universe =>

  val definitions: AbsDefinitions

  abstract class AbsDefinitions {
    // outer packages and their classes
    def RootPackage: Symbol
    def RootClass: Symbol
    def EmptyPackage: Symbol
    def EmptyPackageClass: Symbol

    def ScalaPackage: Symbol
    def ScalaPackageClass: Symbol

    // top types
    def AnyClass   : Symbol
    def AnyValClass: Symbol
    def AnyRefClass: Symbol
    def ObjectClass: Symbol

    // bottom types
    def NullClass   : Symbol
    def NothingClass: Symbol

    // the scala value classes
    def UnitClass   : Symbol
    def ByteClass   : Symbol
    def ShortClass  : Symbol
    def CharClass   : Symbol
    def IntClass    : Symbol
    def LongClass   : Symbol
    def FloatClass  : Symbol
    def DoubleClass : Symbol
    def BooleanClass: Symbol

    // fundamental reference classes
    def SymbolClass : Symbol
    def StringClass : Symbol
    def ClassClass  : Symbol

    // fundamental modules
    def PredefModule: Symbol

    // fundamental type constructions
    def ClassType(arg: Type): Type

    /** The string representation used by the given type in the VM.
     */
    def signature(tp: Type): String

    /** Is symbol one of the value classes? */
    def isValueClass(sym: Symbol): Boolean

    /** Is symbol one of the numeric value classes? */
    def isNumericValueClass(sym: Symbol): Boolean
  }
}
