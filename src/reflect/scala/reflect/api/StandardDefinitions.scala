/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */
package scala.reflect
package api

trait StandardDefinitions extends base.StandardDefinitions {
  self: Universe =>

  val definitions: DefinitionsApi

  trait DefinitionsApi extends DefinitionsBase {
    def JavaLangPackageClass: ClassSymbol
    def JavaLangPackage: ObjectSymbol
    def ArrayObject: ObjectSymbol
    def ArrayObject_overloadedApply: TermSymbol // todo. fix the bug in Definitions.getMemberMethod
    def Array_apply: TermSymbol // todo. fix the bug in Definitions.getMemberMethod
    def Array_clone: TermSymbol // todo. fix the bug in Definitions.getMemberMethod
    def Array_length: TermSymbol // todo. fix the bug in Definitions.getMemberMethod
    def Array_update: TermSymbol // todo. fix the bug in Definitions.getMemberMethod
    def ByNameParamClass: ClassSymbol
    def ConsClass: ClassSymbol
    def FunctionClass : Array[ClassSymbol]
    def IterableClass: ClassSymbol
    def IteratorClass: ClassSymbol
    def IteratorObject: ObjectSymbol
    def Iterator_apply: TermSymbol // todo. fix the bug in Definitions.getMemberMethod
    def JavaRepeatedParamClass: ClassSymbol
    def ListObject: ObjectSymbol
    def List_apply: TermSymbol // todo. fix the bug in Definitions.getMemberMethod
    def NilObject: ObjectSymbol
    def NoneObject: ObjectSymbol
    def OptionClass: ClassSymbol
    def ProductClass  : Array[ClassSymbol]
    def RepeatedParamClass: ClassSymbol
    def SeqClass: ClassSymbol
    def SeqObject: ObjectSymbol
    def SomeClass: ClassSymbol
    def SomeObject: ObjectSymbol
    def StringBuilderClass: ClassSymbol
    def SymbolClass : ClassSymbol
    def TraversableClass: ClassSymbol
    def TupleClass: Array[Symbol] // cannot make it Array[ClassSymbol], because TupleClass(0) is supposed to be NoSymbol. weird
    def ScalaPrimitiveValueClasses: List[ClassSymbol]
    def ScalaNumericValueClasses: List[ClassSymbol]
  }
}
