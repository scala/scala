/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package api

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
  val StringTpe: Type
}

trait StandardDefinitions extends StandardTypes {
  self: Universe =>

  val definitions: AbsDefinitions

  // I intend to pull everything in here out of the public API.
  trait AbsDefinitionsInternal {
    def ArrayModule: Symbol
    def ArrayModule_overloadedApply: Symbol
    def Array_apply: Symbol
    def Array_clone: Symbol
    def Array_length: Symbol
    def Array_update: Symbol
    def ByNameParamClass: Symbol
    def ClassTagModule: Symbol
    def ConcreteTypeTagModule: Symbol
    def ConsClass: Symbol
    def EmptyPackageClass: Symbol
    def FunctionClass : Array[Symbol]
    def IterableClass: Symbol
    def IteratorClass: Symbol
    def IteratorModule: Symbol
    def Iterator_apply: Symbol
    def JavaLangPackageClass: Symbol
    def JavaRepeatedParamClass: Symbol
    def ListModule: Symbol
    def List_apply: Symbol
    def NilModule: Symbol
    def NoneModule: Symbol
    def OptionClass: Symbol
    def ProductClass  : Array[Symbol]
    def RepeatedParamClass: Symbol
    def ScalaPackageClass: Symbol
    def SeqClass: Symbol
    def SeqModule: Symbol
    def SomeClass: Symbol
    def SomeModule: Symbol
    def StringBuilderClass: Symbol
    def SymbolClass : Symbol
    def TraversableClass: Symbol
    def TupleClass    : Array[Symbol]
    def TypeTagModule: Symbol
    def ScalaPrimitiveValueClasses: List[ClassSymbol]
  }

  trait AbsDefinitions extends AbsDefinitionsInternal {
    // packages
    def RootClass: ClassSymbol
    def RootPackage: PackageSymbol
    def EmptyPackage: PackageSymbol
    def ScalaPackage: PackageSymbol
    def JavaLangPackage: PackageSymbol

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
    def ArrayClass: ClassSymbol

    // collections classes
    def ListClass: ClassSymbol
    def ListModule: ModuleSymbol

    // collections modules
    def PredefModule: ModuleSymbol

    // type tags
    def ClassTagClass: ClassSymbol
    def TypeTagClass: ClassSymbol
    def ConcreteTypeTagClass: ClassSymbol

    /** Given a type T, returns the type corresponding to the VM's
     *  representation: ClassClass's type constructor applied to `arg`.
     */
    def vmClassType(arg: Type): Type    // !!! better name?
    // [Eugene] we already have arg.erasure, right?
    //
    // [Paul] You misunderstand the method (it could be better named).
    // Given List[String], it returns java.lang.Class[List[String]]
    // (or the .Net equivalent), not the erasure of List[String].
    // See def ClassType in definitions - that's what it was called before,
    // and obviously that name has to go.

    /** The string representation used by the given type in the VM.
     */
    def vmSignature(sym: Symbol, info: Type): String

    /** Is symbol one of the value classes? */
    def isPrimitiveValueClass(sym: Symbol): Boolean        // !!! better name?

    /** Is symbol one of the numeric value classes? */
    def isNumericValueClass(sym: Symbol): Boolean   // !!! better name?
  }
}
