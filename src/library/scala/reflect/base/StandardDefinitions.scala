/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package base

trait StandardDefinitions {
  self: Universe =>

  val definitions: DefinitionsBase

  trait DefinitionsBase extends StandardTypes {
    // packages
    def ScalaPackageClass: ClassSymbol
    def ScalaPackage: ModuleSymbol

    // top types
    def AnyClass   : ClassSymbol
    def AnyValClass: ClassSymbol
    def ObjectClass: ClassSymbol
    def AnyRefClass: TypeSymbol

    // bottom types
    def NullClass   : ClassSymbol
    def NothingClass: ClassSymbol

    // the scala value classes
    def UnitClass   : ClassSymbol
    def ByteClass   : ClassSymbol
    def ShortClass  : ClassSymbol
    def CharClass   : ClassSymbol
    def IntClass    : ClassSymbol
    def LongClass   : ClassSymbol
    def FloatClass  : ClassSymbol
    def DoubleClass : ClassSymbol
    def BooleanClass: ClassSymbol

    // some special classes
    def StringClass : ClassSymbol
    def ClassClass  : ClassSymbol
    def ArrayClass  : ClassSymbol
    def ListClass   : ClassSymbol

    // the Predef object
    def PredefModule: ModuleSymbol
  }

  trait StandardTypes {
    // the scala value classes
    val UnitTpe: Type
    val ByteTpe: Type
    val ShortTpe: Type
    val CharTpe: Type
    val IntTpe: Type
    val LongTpe: Type
    val FloatTpe: Type
    val DoubleTpe: Type
    val BooleanTpe: Type

    // top types
    val AnyTpe: Type
    val AnyValTpe: Type
    val AnyRefTpe: Type
    val ObjectTpe: Type

    // bottom types
    val NothingTpe: Type
    val NullTpe: Type
  }
}
