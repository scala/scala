/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */
package scala.reflect
package api

/**
 * Defines standard symbols and types.
 */
trait StandardDefinitions {
  self: Universe =>

  /** A value containing all standard defnitions. */
  val definitions: DefinitionsApi

  /** Defines standard symbols (and types via its base trait). */
  trait DefinitionsApi extends StandardTypes {
    /** The class symbol of package `scala`. */
    def ScalaPackageClass: ClassSymbol

    /** The module class symbol of package `scala`. */
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

    /** The class symbol of class `String`. */
    def StringClass : ClassSymbol

    /** The class symbol of class `Class`. */
    def ClassClass  : ClassSymbol

    /** The class symbol of class `Array`. */
    def ArrayClass  : ClassSymbol

    /** The class symbol of class `List`. */
    def ListClass   : ClassSymbol

    /** The module symbol of `scala.Predef`. */
    def PredefModule: ModuleSymbol

    def JavaLangPackageClass: ClassSymbol
    def JavaLangPackage: ModuleSymbol
    def ArrayModule: ModuleSymbol
    def ArrayModule_overloadedApply: TermSymbol // todo. fix the bug in Definitions.getMemberMethod
    def Array_apply: TermSymbol // todo. fix the bug in Definitions.getMemberMethod
    def Array_clone: TermSymbol // todo. fix the bug in Definitions.getMemberMethod
    def Array_length: TermSymbol // todo. fix the bug in Definitions.getMemberMethod
    def Array_update: TermSymbol // todo. fix the bug in Definitions.getMemberMethod
    def ByNameParamClass: ClassSymbol
    def ConsClass: ClassSymbol
    def FunctionClass : Array[ClassSymbol]
    def IterableClass: ClassSymbol
    def IteratorClass: ClassSymbol
    def IteratorModule: ModuleSymbol
    def Iterator_apply: TermSymbol // todo. fix the bug in Definitions.getMemberMethod
    def JavaRepeatedParamClass: ClassSymbol
    def ListModule: ModuleSymbol
    def List_apply: TermSymbol // todo. fix the bug in Definitions.getMemberMethod
    def NilModule: ModuleSymbol
    def NoneModule: ModuleSymbol
    def OptionClass: ClassSymbol
    def ProductClass  : Array[ClassSymbol]
    def RepeatedParamClass: ClassSymbol
    def SeqClass: ClassSymbol
    def SeqModule: ModuleSymbol
    def SomeClass: ClassSymbol
    def SomeModule: ModuleSymbol
    def StringBuilderClass: ClassSymbol
    def SymbolClass : ClassSymbol
    def TraversableClass: ClassSymbol
    def TupleClass: Array[Symbol] // cannot make it Array[ClassSymbol], because TupleClass(0) is supposed to be NoSymbol. weird
    def ScalaPrimitiveValueClasses: List[ClassSymbol]
    def ScalaNumericValueClasses: List[ClassSymbol]
  }

  /** Defines standard types. */
  trait StandardTypes {
    /** The `Type` of type `Unit`. */
    val UnitTpe: Type

    /** The `Type` of primitive type `Byte`. */
    val ByteTpe: Type

    /** The `Type` of primitive type `Short`. */
    val ShortTpe: Type

    /** The `Type` of primitive type `Char`. */
    val CharTpe: Type

    /** The `Type` of primitive type `Int`. */
    val IntTpe: Type

    /** The `Type` of primitive type `Long`. */
    val LongTpe: Type

    /** The `Type` of primitive type `Float`. */
    val FloatTpe: Type

    /** The `Type` of primitive type `Double`. */
    val DoubleTpe: Type

    /** The `Type` of primitive type `Boolean`. */
    val BooleanTpe: Type

    /** The `Type` of type `Any`. */
    val AnyTpe: Type

    /** The `Type` of type `AnyVal`. */
    val AnyValTpe: Type

    /** The `Type` of type `AnyRef`. */
    val AnyRefTpe: Type

    /** The `Type` of type `Object`. */
    val ObjectTpe: Type

    /** The `Type` of type `Nothing`. */
    val NothingTpe: Type

    /** The `Type` of type `Null`. */
    val NullTpe: Type
  }
}
