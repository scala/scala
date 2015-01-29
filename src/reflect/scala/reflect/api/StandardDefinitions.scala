/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */
package scala
package reflect
package api

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 * All Scala standard symbols and types.
 *
 * These standard definitions can accessed to using `definitions`.
 * They're typically imported with a wildcard import, `import definitions._`, and are
 * listed in [[scala.reflect.api.StandardDefinitions#DefinitionsApi]].
 *
 *  @group ReflectionAPI
 */
trait StandardDefinitions {
  self: Universe =>

  /** A value containing all standard definitions in [[DefinitionsApi]]
   *  @group Definitions
   */
  val definitions: DefinitionsApi

  /** Defines standard symbols (and types via its base trait).
   *  @group API
   */
  trait DefinitionsApi extends StandardTypes {
    /** The module class symbol of package `scala`. */
    def ScalaPackageClass: ClassSymbol

    /** The module symbol of package `scala`. */
    def ScalaPackage: ModuleSymbol

    /** The class symbol of core class `scala.Any`. */
    def AnyClass   : ClassSymbol

    /** The class symbol of core class `scala.AnyVal`. */
    def AnyValClass: ClassSymbol

    /** The class symbol of core class `java.lang.Object`. */
    def ObjectClass: ClassSymbol

    /** The type symbol of core class `scala.AnyRef`. */
    def AnyRefClass: TypeSymbol

    /** The class symbol of core class `scala.Null`. */
    def NullClass   : ClassSymbol

    /** The class symbol of core class `scala.Nothing`. */
    def NothingClass: ClassSymbol

    /** The class symbol of primitive class `scala.Unit`. */
    def UnitClass   : ClassSymbol

    /** The class symbol of primitive class `scala.Byte`. */
    def ByteClass   : ClassSymbol

    /** The class symbol of primitive class `scala.Short`. */
    def ShortClass  : ClassSymbol

    /** The class symbol of primitive class `scala.Char`. */
    def CharClass   : ClassSymbol

    /** The class symbol of primitive class `scala.Int`. */
    def IntClass    : ClassSymbol

    /** The class symbol of primitive class `scala.Long`. */
    def LongClass   : ClassSymbol

    /** The class symbol of primitive class `scala.Float`. */
    def FloatClass  : ClassSymbol

    /** The class symbol of primitive class `scala.Double`. */
    def DoubleClass : ClassSymbol

    /** The class symbol of primitive class `scala.Boolean`. */
    def BooleanClass: ClassSymbol

    /** The class symbol of class `scala.String`. */
    def StringClass : ClassSymbol

    /** The class symbol of class `java.lang.Class`. */
    def ClassClass  : ClassSymbol

    /** The class symbol of class `scala.Array`. */
    def ArrayClass  : ClassSymbol

    /** The class symbol of class `scala.List`. */
    def ListClass   : ClassSymbol

    /** The module symbol of module `scala.Predef`. */
    def PredefModule: ModuleSymbol

    /** The module class symbol of package `java.lang`. */
    def JavaLangPackageClass: ClassSymbol

    /** The module symbol of package `java.lang`. */
    def JavaLangPackage: ModuleSymbol

    /** The module symbol of module `scala.Array`. */
    def ArrayModule: ModuleSymbol

    /** The method symbol of method `apply` in module `scala.Array`. */
    def ArrayModule_overloadedApply: TermSymbol // todo. fix the bug in Definitions.getMemberMethod

    /** The method symbol of method `apply` in class `scala.Array`. */
    def Array_apply: TermSymbol // todo. fix the bug in Definitions.getMemberMethod

    /** The method symbol of method `clone` in class `scala.Array`. */
    def Array_clone: TermSymbol // todo. fix the bug in Definitions.getMemberMethod

    /** The method symbol of method `length` in class `scala.Array`. */
    def Array_length: TermSymbol // todo. fix the bug in Definitions.getMemberMethod

    /** The method symbol of method `update` in class `scala.Array`. */
    def Array_update: TermSymbol // todo. fix the bug in Definitions.getMemberMethod

    /** A dummy class symbol that is used to indicate by-name parameters.
     *
     *  {{{
     *  scala> class C { def m(x: => Int) = ??? }
     *  defined class C
     *
     *  scala> import scala.reflect.runtime.universe._
     *  import scala.reflect.runtime.universe._
     *
     *  scala> val m = typeOf[C].member(TermName("m")).asMethod
     *  m: reflect.runtime.universe.MethodSymbol = method m
     *
     *  scala> m.params(0)(0).info
     *  res1: reflect.runtime.universe.Type = => scala.Int
     *
     *  scala> showRaw(m.params(0)(0).info)
     *  res2: String = TypeRef(
     *      ThisType(scala),
     *      scala.<byname>, // <-- ByNameParamClass
     *      List(TypeRef(ThisType(scala), scala.Int, List())))
     *  }}}
     */
    def ByNameParamClass: ClassSymbol

    /** A dummy class symbol that is used to indicate repeated parameters
     *  compiled by the Java compiler.
     *
     *  {{{
     *  class C {
     *    public void m(Object... x) {}
     *  }
     *  }}}
     *
     *  {{{
     *  scala> import scala.reflect.runtime.universe._
     *  import scala.reflect.runtime.universe._
     *
     *  scala> val m = typeOf[C].member(TermName("m")).asMethod
     *  m: reflect.runtime.universe.MethodSymbol = method m
     *
     *  scala> m.params(0)(0).info
     *  res1: reflect.runtime.universe.Type = <repeated...>[Object]
     *
     *  scala> showRaw(m.params(0)(0).info)
     *  res2: String = TypeRef(
     *      ThisType(scala),
     *      scala.<repeated...>, // <-- JavaRepeatedParamClass
     *      List(TypeRef(ThisType(java.lang), Object, List())))
     *  }}}
     */
    def JavaRepeatedParamClass: ClassSymbol

    /** A dummy class symbol that is used to indicate repeated parameters
     *  compiled by the Scala compiler.
     *
     *  {{{
     *  scala> class C { def m(x: Int*) = ??? }
     *  defined class C
     *
     *  scala> import scala.reflect.runtime.universe._
     *  import scala.reflect.runtime.universe._
     *
     *  scala> val m = typeOf[C].member(TermName("m")).asMethod
     *  m: reflect.runtime.universe.MethodSymbol = method m
     *
     *  scala> m.params(0)(0).info
     *  res1: reflect.runtime.universe.Type = scala.Int*
     *
     *  scala> showRaw(m.params(0)(0).info)
     *  res2: String = TypeRef(
     *      ThisType(scala),
     *      scala.<repeated>, // <-- RepeatedParamClass
     *      List(TypeRef(ThisType(scala), scala.Int, List())))
     *  }}}
     */
    def RepeatedParamClass: ClassSymbol

    /** The module symbol of module `scala.List`. */
    def ListModule: ModuleSymbol

    /** The method symbol of method `apply` in class `scala.List`. */
    def List_apply: TermSymbol // todo. fix the bug in Definitions.getMemberMethod

    /** The module symbol of module `scala.collection.immutable.Nil`. */
    def NilModule: ModuleSymbol

    /** The class symbol of class `scala.Option`. */
    def OptionClass: ClassSymbol

    /** The module symbol of module `scala.None`. */
    def NoneModule: ModuleSymbol

    /** The module symbol of module `scala.Some`. */
    def SomeModule: ModuleSymbol

    /** Function-like api that lets you acess symbol
     *  of the definition with given arity and also look
     *  through all known symbols via `seq`.
     */
    abstract class VarArityClassApi extends (Int => Symbol) {
      def seq: Seq[ClassSymbol]
    }

    /** Function-like object that maps arity to symbols for classes `scala.ProductX`.
     *   -  0th element is `Unit`
     *   -  1st element is `Product1`
     *   -  ...
     *   - 22nd element is `Product22`
     *   - 23nd element is `NoSymbol`
     *   - ...
     */
    def ProductClass: VarArityClassApi

    /** Function-like object that maps arity to symbols for classes `scala.FunctionX`.
     *   -  0th element is `Function0`
     *   -  1st element is `Function1`
     *   -  ...
     *   - 22nd element is `Function22`
     *   - 23nd element is `NoSymbol`
     *   - ...
     */
    def FunctionClass: VarArityClassApi

    /** Function-like object that maps arity to symbols for classes `scala.TupleX`.
     *   -  0th element is `NoSymbol`
     *   -  1st element is `Tuple1`
     *   -  ...
     *   - 22nd element is `Tuple22`
     *   - 23nd element is `NoSymbol`
     *   - ...
     */
    def TupleClass: VarArityClassApi

    /** Contains Scala primitive value classes:
     *   - Byte
     *   - Short
     *   - Int
     *   - Long
     *   - Float
     *   - Double
     *   - Char
     *   - Boolean
     *   - Unit
     */
    def ScalaPrimitiveValueClasses: List[ClassSymbol]

    /** Contains Scala numeric value classes:
     *   - Byte
     *   - Short
     *   - Int
     *   - Long
     *   - Float
     *   - Double
     *   - Char
     */
    def ScalaNumericValueClasses: List[ClassSymbol]
  }

  /** Defines standard types.
   *  @group Definitions
   */
  trait StandardTypes {
    /** The type of primitive type `Unit`. */
    val UnitTpe: Type

    /** The type of primitive type `Byte`. */
    val ByteTpe: Type

    /** The type of primitive type `Short`. */
    val ShortTpe: Type

    /** The type of primitive type `Char`. */
    val CharTpe: Type

    /** The type of primitive type `Int`. */
    val IntTpe: Type

    /** The type of primitive type `Long`. */
    val LongTpe: Type

    /** The type of primitive type `Float`. */
    val FloatTpe: Type

    /** The type of primitive type `Double`. */
    val DoubleTpe: Type

    /** The type of primitive type `Boolean`. */
    val BooleanTpe: Type

    /** The type of core type `Any`. */
    val AnyTpe: Type

    /** The type of core type `AnyVal`. */
    val AnyValTpe: Type

    /** The type of core type `AnyRef`. */
    val AnyRefTpe: Type

    /** The type of core type `Object`. */
    val ObjectTpe: Type

    /** The type of core type `Nothing`. */
    val NothingTpe: Type

    /** The type of core type `Null`. */
    val NullTpe: Type
  }
}
