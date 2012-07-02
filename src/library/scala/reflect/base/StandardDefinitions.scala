/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package base

// [Eugene++] not sure whether we need this in the top level of the universe
trait StandardTypes {
  self: Universe =>

  val ByteTpe: Type
  val ShortTpe: Type
  val CharTpe: Type
  val IntTpe: Type
  val LongTpe: Type
  val FloatTpe: Type
  val DoubleTpe: Type
  val BooleanTpe: Type
  val UnitTpe: Type

  val AnyTpe: Type
  val AnyValTpe: Type
  val AnyRefTpe: Type
  val ObjectTpe: Type

  val NothingTpe: Type
  val NullTpe: Type
}

trait StandardDefinitions extends StandardTypes {
  self: Universe =>

  val definitions: DefinitionsBase

  // [Eugene] todo. shortcut to these fields if possible when generating tags
  // todo. also shortcut to StandardTypes, of course
  trait DefinitionsBase {
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
    def ListClass   : ClassSymbol // [Eugene] I'd say List has earned its right to be here

    // the Predef object
    def PredefModule: ModuleSymbol
  }
}
