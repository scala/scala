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
}
