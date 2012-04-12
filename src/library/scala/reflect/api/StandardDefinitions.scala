/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package api

trait StandardDefinitions { self: Universe =>

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
  val ObjectTpe: Type
  val AnyValTpe: Type
  val AnyRefTpe: Type
  val NothingTpe: Type
  val NullTpe: Type
  val StringTpe: Type

  val definitions: AbsDefinitions

  abstract class AbsDefinitions {
    // packages
    def RootPackage: Symbol
    def RootClass: Symbol
    def EmptyPackage: Symbol
    def EmptyPackageClass: Symbol
    def ScalaPackage: Symbol
    def ScalaPackageClass: Symbol
    def JavaLangPackage: Symbol
    def JavaLangPackageClass: Symbol

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
    def ScalaPrimitiveValueClasses: List[Symbol]

    // fundamental reference classes
    def SymbolClass : Symbol
    def StringClass : Symbol
    def ClassClass  : Symbol

    // product, tuple, function
    def TupleClass    : Array[Symbol]
    def ProductClass  : Array[Symbol]
    def FunctionClass : Array[Symbol]

    // Option classes
    def OptionClass: Symbol
    def SomeClass: Symbol
    def NoneModule: Symbol
    def SomeModule: Symbol

    // collections classes
    def ConsClass: Symbol
    def IterableClass: Symbol
    def IteratorClass: Symbol
    def ListClass: Symbol
    def SeqClass: Symbol
    def StringBuilderClass: Symbol
    def TraversableClass: Symbol

    // collections modules
    def PredefModule: Symbol
    def ListModule: Symbol
    def List_apply: Symbol
    def NilModule: Symbol
    def SeqModule: Symbol
    def IteratorModule: Symbol
    def Iterator_apply: Symbol

    // arrays and their members
    def ArrayModule: Symbol
    def ArrayModule_overloadedApply: Symbol
    def ArrayClass: Symbol
    def Array_apply: Symbol
    def Array_update: Symbol
    def Array_length: Symbol
    def Array_clone: Symbol

    // special parameter types
    def ByNameParamClass: Symbol
    def JavaRepeatedParamClass: Symbol
    def RepeatedParamClass: Symbol

    // type tags
    def ClassTagClass: Symbol
    def ClassTagModule: Symbol
    def TypeTagClass: Symbol
    def TypeTagModule: Symbol
    def ConcreteTypeTagClass: Symbol
    def ConcreteTypeTagModule: Symbol

    /** Given a type T, returns the type corresponding to the VM's
     *  representation: ClassClass's type constructor applied to `arg`.
     */
    def vmClassType(arg: Type): Type    // !!! better name?
                                        // [Eugene] we already have arg.erasure, right?

    /** The string representation used by the given type in the VM.
     */
    def vmSignature(sym: Symbol, info: Type): String

    /** Is symbol one of the value classes? */
    def isPrimitiveValueClass(sym: Symbol): Boolean        // !!! better name?

    /** Is symbol one of the numeric value classes? */
    def isNumericValueClass(sym: Symbol): Boolean   // !!! better name?
  }
}
