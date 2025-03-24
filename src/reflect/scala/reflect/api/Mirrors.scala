/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect
package api

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 * This trait provides support for Mirrors in the Scala Reflection API.
 *
 * `Mirror`s are a central part of Scala Reflection. All information provided by
 * reflection is made accessible through `Mirror`s. Depending on the type of information
 * to be obtained, or the reflective action to be taken, different flavors of mirrors
 * must be used. "Classloader" mirrors can be used to obtain representations of types
 * and members. From a classloader `Mirror`, it's possible to obtain more specialized
 * "invoker" `Mirror`s (the most commonly-used mirrors), which implement reflective
 * invocations, such as method/constructor calls and field accesses.
 *
 * The two flavors of mirrors:
 *
 * <ul>
 * <li>'''“Classloader” mirrors'''. These mirrors translate names to symbols
 * (via methods `staticClass`/`staticModule`/`staticPackage`).</li>
 * <li>'''"Invoker” mirrors'''. These mirrors implement reflective invocations
 * (via methods `MethodMirror.apply`, `FieldMirror.get`, etc). These "invoker"
 * mirrors are the types of mirrors that are most commonly used.</li>
 * </ul>
 *
 * === Compile-time Mirrors ===
 * Compile-time `Mirror`s make use of only classloader `Mirror`s to load `Symbol`s
 * by name.
 *
 * The entry point to classloader `Mirror`s is via [[scala.reflect.macros.blackbox.Context#mirror]] or [[scala.reflect.macros.whitebox.Context#mirror]].
 * Typical methods which use classloader `Mirror`s include [[scala.reflect.api.Mirror#staticClass]],
 * [[scala.reflect.api.Mirror#staticModule]], and [[scala.reflect.api.Mirror#staticPackage]]. For
 * example:
 * {{{
 *  import scala.reflect.macros.blackbox.Context
 *
 *  case class Location(filename: String, line: Int, column: Int)
 *
 *  object Macros {
 *    def currentLocation: Location = macro impl
 *
 *    def impl(c: Context): c.Expr[Location] = {
 *      import c.universe._
 *      val pos = c.macroApplication.pos
 *      val clsLocation = c.mirror.staticModule("Location") // get symbol of "Location" object
 *      c.Expr(Apply(Ident(clsLocation), List(Literal(Constant(pos.source.path)), Literal(Constant(pos.line)), Literal(Constant(pos.column)))))
 *    }
 *  }
 * }}}
 *
 * ''Of Note:'' There are several high-level alternatives that one can use to avoid having to manually
 * lookup symbols. For example, `typeOf[Location.type].termSymbol` (or `typeOf[Location].typeSymbol`
 * if we needed a `ClassSymbol`), which are type safe since we don’t have to use `String`s to lookup
 * the `Symbol`.
 *
 * === Runtime Mirrors ===
 *
 * Runtime `Mirror`s make use of both classloader and invoker `Mirror`s.
 *
 * The entry point to `Mirror`s for use at runtime is via `ru.runtimeMirror(<classloader>)`, where
 * `ru` is [[scala.reflect.runtime.universe]].
 *
 * The result of a [[scala.reflect.api.JavaUniverse#runtimeMirror]] call is a classloader mirror,
 * of type [[scala.reflect.api.Mirrors#ReflectiveMirror]], which can load symbols by names as
 * discussed above (in the “Compile-time” section).
 *
 * A classloader mirror can create invoker mirrors, which include: [[scala.reflect.api.Mirrors#InstanceMirror]],
 * [[scala.reflect.api.Mirrors#MethodMirror]], [[scala.reflect.api.Mirrors#FieldMirror]],
 * [[scala.reflect.api.Mirrors#ClassMirror]] and [[scala.reflect.api.Mirrors#ModuleMirror]].
 *
 * Examples of how these two types of `Mirror`s interact are available below.
 *
 * === Types of Mirrors, Their Use Cases & Examples ===
 *
 * '''[[scala.reflect.api.Mirrors#ReflectiveMirror]]'''. Used for loading `Symbol`s by name, and
 * as an entry point into invoker mirrors. Entry point: `val m = ru.runtimeMirror(<classloader>)`.
 * Example:
 * {{{
 *   scala> val ru = scala.reflect.runtime.universe
 *   ru: scala.reflect.api.JavaUniverse = ...
 *
 *   scala> val m = ru.runtimeMirror(getClass.getClassLoader)
 *   m: reflect.runtime.universe.Mirror = JavaMirror ...
 * }}}
 *
 * '''[[scala.reflect.api.Mirrors#InstanceMirror]]'''. Used for creating invoker `Mirror`s for methods
 * and fields and for inner classes and inner objects (modules). Entry point: `val im = m.reflect(<value>)`.
 * Example:
 * {{{
 *   scala> class C { def x = 2 }
 *   defined class C
 *
 *   scala> val im = m.reflect(new C)
 *   im: reflect.runtime.universe.InstanceMirror = instance mirror for C@3442299e
 * }}}
 *
 * '''[[scala.reflect.api.Mirrors#MethodMirror]]'''. Used for invoking instance methods (Scala only has
 * instance methods-- methods of objects are instance methods of object instances, obtainable
 * via `ModuleMirror.instance`). Entry point: `val mm = im.reflectMethod(<method symbol>)`.
 * Example:
 * {{{
 *   scala> val methodX = typeOf[C].decl(TermName("x")).asMethod
 *   methodX: reflect.runtime.universe.MethodSymbol = method x
 *
 *   scala> val mm = im.reflectMethod(methodX)
 *   mm: reflect.runtime.universe.MethodMirror = method mirror for C.x: scala.Int (bound to C@3442299e)
 *
 *   scala> mm()
 *   res0: Any = 2
 * }}}
 *
 * '''[[scala.reflect.api.Mirrors#FieldMirror]]'''. Used for getting/setting instance fields
 * (Scala only has instance fields-- fields of objects are instance methods of object instances
 * obtainable via ModuleMirror.instance). Entry point:
 * `val fm = im.reflectMethod(<field or accessor symbol>)`.
 * Example:
 * {{{
 *   scala> class C { val x = 2; val y = 3 }
 *   defined class C
 *
 *   scala> val m = ru.runtimeMirror(getClass.getClassLoader)
 *   m: reflect.runtime.universe.Mirror = JavaMirror ...
 *
 *   scala> val im = m.reflect(new C)
 *   im: reflect.runtime.universe.InstanceMirror = instance mirror for C@5f0c8ac1
 *
 *   scala> val fieldX = typeOf[C].decl(TermName("x")).asTerm.accessed.asTerm
 *   fieldX: reflect.runtime.universe.TermSymbol = value x
 *   scala> val fmX = im.reflectField(fieldX)
 *   fmX: reflect.runtime.universe.FieldMirror = field mirror for C.x (bound to C@5f0c8ac1)
 *
 *   scala> fmX.get
 *   res0: Any = 2
 *
 *   scala> fmX.set(3) // NOTE: can set an underlying value of an immutable field!
 *
 *   scala> val fieldY = typeOf[C].decl(TermName("y")).asTerm.accessed.asTerm
 *   fieldY: reflect.runtime.universe.TermSymbol = variable y
 *
 *   scala> val fmY = im.reflectField(fieldY)
 *   fmY: reflect.runtime.universe.FieldMirror = field mirror for C.y (bound to C@5f0c8ac1)
 *
 *   scala> fmY.get
 *   res1: Any = 3
 *
 *   scala> fmY.set(4)
 *
 *   scala> fmY.get
 *   res2: Any = 4
 * }}}
 *
 * '''[[scala.reflect.api.Mirrors#ClassMirror]]'''. Used for creating invoker mirrors for constructors.
 * Entry points: for ''static classes'' `val cm1 = m.reflectClass(<class symbol>)`,
 * for ''inner classes'' `val mm2 = im.reflectClass(<class symbol>)`.
 * Example:
 * {{{
 *   scala> case class C(x: Int)
 *   defined class C
 *
 *   scala> val m = ru.runtimeMirror(getClass.getClassLoader)
 *   m: reflect.runtime.universe.Mirror = JavaMirror ...
 *
 *   scala> val classC = typeOf[C].typeSymbol.asClass
 *
 *   classC: reflect.runtime.universe.Symbol = class C
 *
 *   scala> val cm = m.reflectClass(classC)
 *   cm: reflect.runtime.universe.ClassMirror = class mirror for C (bound to null)
 *
 *   scala> val ctorC = typeOf[C].decl(ru.nme.CONSTRUCTOR).asMethod
 *   ctorC: reflect.runtime.universe.MethodSymbol = constructor C
 *
 *   scala> val ctorm = cm.reflectConstructor(ctorC)
 *   ctorm: reflect.runtime.universe.MethodMirror = constructor mirror for C.<init>(x: scala.Int): C (bound to null)
 *
 *   scala> ctorm(2)
 *   res0: Any = C(2)
 * }}}
 *
 * '''[[scala.reflect.api.Mirrors#ModuleMirror]]'''. Used for getting singleton instances of objects.
 * Entry points: for ''static objects (modules)'' `val mm1 = m.reflectModule(<module symbol>)`,
 * for ''inner objects (modules)'' `val mm2 = im.reflectModule(<module symbol>)`.
 * Example:
 * {{{
 *   scala> object C { def x = 2 }
 *   defined module C
 *
 *   scala> val m = ru.runtimeMirror(getClass.getClassLoader)
 *   m: reflect.runtime.universe.Mirror = JavaMirror ...
 *
 *   scala> val objectC = typeOf[C.type].termSymbol.asModule
 *   objectC: reflect.runtime.universe.ModuleSymbol = object C
 *
 *   scala> val mm = m.reflectModule(objectC)
 *   mm: reflect.runtime.universe.ModuleMirror = module mirror for C (bound to null)
 *
 *   scala> val obj = mm.instance
 *   obj: Any = C$@1005ec04
 * }}}
 *
 * For more information about `Mirrors`s, see the
 * [[https://docs.scala-lang.org/overviews/reflection/environment-universes-mirrors.html Reflection Guide: Mirrors]]
 *
 *  @contentDiagram hideNodes "*Api"
 *  @group ReflectionAPI
 */
trait Mirrors { self: Universe =>

  /** The base type of all mirrors of this universe.
   *
   *  This abstract type conforms the base interface for all mirrors defined in [[scala.reflect.api.Mirror]]
   *  and is gradually refined in specific universes (e.g. `Mirror` of a [[scala.reflect.api.JavaUniverse]] is capable of reflection).
   *  @group Mirrors
   */
  type Mirror >: Null <: scala.reflect.api.Mirror[self.type]

  /** The root mirror of this universe. This mirror contains standard Scala classes and types such as `Any`, `AnyRef`, `AnyVal`,
   *  `Nothing`, `Null`, and all classes loaded from scala-library, which are shared across all mirrors within the enclosing universe.
   *  @group Mirrors
   */
  val rootMirror: Mirror

  /** Abstracts the runtime representation of a class on the underlying platform.
   *  @group Mirrors
   */
  type RuntimeClass >: Null <: AnyRef

  /** Has no special methods. Is here to provides erased identity for `RuntimeClass`.
   *  @group API
   */
  trait RuntimeClassApi

  // todo. an improvement might be having mirrors reproduce the structure of the reflection domain
  // e.g. a ClassMirror could also have a list of fields, methods, constructors and so on
  // read up more on the proposed design in "Reflecting Scala" by Y. Coppel

  /** A mirror that reflects a runtime value.
   *  See [[scala.reflect.api.package the overview page]] for details on how to use runtime reflection.
   *  @group Mirrors
   */
  trait InstanceMirror {

    /** The instance value reflected by this mirror */
    def instance: Any

    /** The symbol corresponding to the runtime class of the reflected instance */
    def symbol: ClassSymbol

    /** Reflects against a field symbol and returns a mirror
     *  that can be used to get and, if appropriate, set the value of the field.
     *
     *  FieldMirrors are the only way to get at private[this] vals and vars and
     *  might be useful to inspect the data of underlying Java fields.
     *  For all other uses, it's better to go through the fields accessor.
     *
     *  In particular, there should be no need to ever access a field mirror
     *  when reflecting on just the public members of a class or trait.
     *  Note also that only accessor MethodMirrors, but not FieldMirrors will accurately reflect overriding behavior.
     *
     *  To get a field symbol by the name of the field you would like to reflect,
     *  use `<this mirror>.symbol.info.member(TermName(<name of the field>)).asTerm.accessed`.
     *  For further information about member lookup refer to `Symbol.info`.
     *
     *  The input symbol can be either private or non-private (Scala reflection transparently deals with visibility).
     *  It must be a member (declared or inherited) of the class of the instance underlying this mirror.
     *
     *  The input symbol can represent either a field itself or one of the corresponding accessors
     *  (in all cases the resulting mirror will refer to the field symbol).
     *
     *  If a field symbol doesn't correspond to a reflectable entity of the underlying platform,
     *  a `ScalaReflectionException` exception will be thrown. This might happen, for example, for primary constructor parameters.
     *  Typically they produce class fields, however, private parameters that aren't used outside the constructor
     *  remain plain parameters of a constructor method of the class.
     */
    def reflectField(field: TermSymbol): FieldMirror

    /** Reflects against a method symbol and returns a mirror
     *  that can be used to invoke the method provided.
     *
     *  To get a method symbol by the name of the method you would like to reflect,
     *  use `<this mirror>.symbol.info.member(TermName(<name of the method>)).asMethod`.
     *  For further information about member lookup refer to `Symbol.info`.
     *
     *  The input symbol can be either private or non-private (Scala reflection transparently deals with visibility).
     *  It must be a member (declared or inherited) of the instance underlying this mirror.
     */
    def reflectMethod(method: MethodSymbol): MethodMirror

    /** Reflects against an inner class symbol and returns a mirror
     *  that can be used to create instances of the class, inspect its companion object or perform further reflections.
     *
     *  To get a class symbol by the name of the class you would like to reflect,
     *  use `<this mirror>.symbol.info.member(TypeName(<name of the class>)).asClass`.
     *  For further information about member lookup refer to `Symbol.info`.
     *
     *  The input symbol can be either private or non-private (Scala reflection transparently deals with visibility).
     *  It must be a member (declared or inherited) of the instance underlying this mirror.
     */
    def reflectClass(cls: ClassSymbol): ClassMirror

    /** Reflects against an inner module symbol and returns a mirror
     *  that can be used to get the instance of the object or inspect its companion class.
     *
     *  To get a module symbol by the name of the object you would like to reflect,
     *  use `<this mirror>.symbol.info.member(TermName(<name of the object>)).asModule`.
     *  For further information about member lookup refer to `Symbol.info`.
     *
     *  The input symbol can be either private or non-private (Scala reflection transparently deals with visibility).
     *  It must be a member (declared or inherited) of the instance underlying this mirror.
     */
    def reflectModule(mod: ModuleSymbol): ModuleMirror
  }

  /** A mirror that reflects a field.
   *  See [[scala.reflect.api.package the overview page]] for details on how to use runtime reflection.
   *  @group Mirrors
   */
  trait FieldMirror {

    /** The object containing the field */
    def receiver: Any

    /** The field symbol representing the field.
     *
     *  In Scala `val` and `var` declarations are usually compiled down to a pair of
     *  a backing field and corresponding accessor/accessors, which means that a single
     *  declaration might correspond to up to three different symbols. Nevertheless
     *  the `FieldMirror.symbol` field always points to a backing field symbol.
     */
    def symbol: TermSymbol

    /** Retrieves the value stored in the field.
     *
     *  Scala reflection uses reflection capabilities of the underlying platform,
     *  so `FieldMirror.get` might throw platform-specific exceptions associated
     *  with getting a field or invoking a getter method of the field.
     *
     *  If `symbol` represents a field of a base class with respect to the class of the receiver,
     *  and this base field is overridden in the class of the receiver, then this method will retrieve
     *  the value of the base field. To achieve overriding behavior, use reflectMethod on an accessor.
     */
    def get: Any

    /** Updates the value stored in the field.
     *
     *  If a field is immutable, a `ScalaReflectionException` will be thrown.
     *
     *  Scala reflection uses reflection capabilities of the underlying platform,
     *  so `FieldMirror.get` might throw platform-specific exceptions associated
     *  with setting a field or invoking a setter method of the field.
     *
     *  If `symbol` represents a field of a base class with respect to the class of the receiver,
     *  and this base field is overridden in the class of the receiver, then this method will set
     *  the value of the base field. To achieve overriding behavior, use reflectMethod on an accessor.
     */
    def set(value: Any): Unit

    /** Creates a new mirror which uses the same symbol, but is bound to a different receiver.
     *  This is significantly faster than recreating the mirror from scratch.
     */
    def bind(newReceiver: Any): FieldMirror
  }

  /** A mirror that reflects a method.
   *  See [[scala.reflect.api.package the overview page]] for details on how to use runtime reflection.
   *  @group Mirrors
   */
  trait MethodMirror {

    /** The receiver object of the method */
    def receiver: Any

    /** The method symbol representing the method */
    def symbol: MethodSymbol

    /** The result of applying the method to the given arguments
     *
     *  Scala reflection uses reflection capabilities of the underlying platform,
     *  so `FieldMirror.get` might throw platform-specific exceptions associated
     *  with invoking the corresponding method or constructor.
     */
    def apply(args: Any*): Any

    /** Creates a new mirror which uses the same symbol, but is bound to a different receiver.
     *  This is significantly faster than recreating the mirror from scratch.
     */
    def bind(newReceiver: Any): MethodMirror
  }

  /** A mirror that reflects the instance or static parts of a runtime class.
   *  See [[scala.reflect.api.package the overview page]] for details on how to use runtime reflection.
   *  @group Mirrors
   */
  trait TemplateMirror {

    /** True if the mirror represents the static part
     *  of a runtime class or the companion object of a Scala class.
     *  One has:
     *
     *    this.isStatic == this.isInstanceOf[ModuleMirror]
     *    !this.isStatic == this.isInstanceOf[ClassMirror]
     */
    def isStatic: Boolean

    /** The Scala symbol corresponding to the reflected runtime class or object */
    def symbol: Symbol
  }

  /** A mirror that reflects a Scala object definition or the static parts of a runtime class.
   *  See [[scala.reflect.api.package the overview page]] for details on how to use runtime reflection.
   *  @group Mirrors
   */
  trait ModuleMirror extends TemplateMirror {

    /** The Scala module symbol corresponding to the reflected object */
    override def symbol: ModuleSymbol

    /** If the reflected runtime class corresponds to a Scala object definition,
     *  returns the single instance representing that object.
     *  If this mirror reflects the static part of a runtime class, returns `null`.
     */
    def instance: Any
  }

  /** A mirror that reflects the instance parts of a runtime class.
   *  See [[scala.reflect.api.package the overview page]] for details on how to use runtime reflection.
   *  @group Mirrors
   */
  trait ClassMirror extends TemplateMirror {

    /** The Scala class symbol corresponding to the reflected class */
    override def symbol: ClassSymbol

    /** Reflects against a constructor symbol and returns a mirror
     *  that can be used to invoke it and construct instances of this mirror's symbols.
     *
     *  To get a constructor symbol you would like to reflect,
     *  use `<this mirror>.symbol.info.member(termNames.CONSTRUCTOR).asMethod`.
     *  For further information about member lookup refer to `Symbol.info`.
     *
     *  The input symbol can be either private or non-private (Scala reflection transparently deals with visibility).
     *  It must be a member (declared or inherited) of the class underlying this mirror.
     */
    def reflectConstructor(constructor: MethodSymbol): MethodMirror
  }

  /** A mirror that reflects instances and static classes.
   *  See [[scala.reflect.api.package the overview page]] for details on how to use runtime reflection.
   *  @group Mirrors
   */
  trait ReflectiveMirror extends scala.reflect.api.Mirror[Mirrors.this.type] {

    /** A reflective mirror for the given object.
     *
     *  Such a mirror can be used to further reflect against the members of the object
     *  to get/set fields, invoke methods and inspect inner classes and objects.
     */
    // we need a ClassTag here to preserve boxity of primitives
    // the class tag lets us tell apart `mirror.reflect(2)` and `mirror.reflect(new Integer(2))`
    def reflect[T: ClassTag](obj: T): InstanceMirror

    /** Reflects against a static class symbol and returns a mirror
     *  that can be used to create instances of the class, inspect its companion object or perform further reflections.
     *
     *  To get a class symbol by the name of the class you would like to reflect,
     *  use `<this mirror>.classSymbol(<runtime class loaded by its name>)`.
     *
     *  The input symbol can be either private or non-private (Scala reflection transparently deals with visibility).
     *  It must be static, i.e. either top-level or nested within one or several static objects.
     */
    def reflectClass(cls: ClassSymbol): ClassMirror

    /** Reflects against a static module symbol and returns a mirror
     *  that can be used to get the instance of the object or inspect its companion class.
     *
     *  To get a module symbol by the name of its companion class you would like to reflect,
     *  use `<this mirror>.classSymbol(<runtime class loaded by its name>).companion.get`.
     *
     *  The input symbol can be either private or non-private (Scala reflection transparently deals with visibility).
     *  It must be static, i.e. either top-level or nested within one or several static objects.
     */
    def reflectModule(mod: ModuleSymbol): ModuleMirror
  }

  /** The API of a mirror for a reflective universe.
   *  See [[scala.reflect.api.package the overview page]] for details on how to use runtime reflection.
   *  @group Mirrors
   */
  trait RuntimeMirror extends ReflectiveMirror { self =>

    /** Maps a Scala type to the corresponding Java class object */
    def runtimeClass(tpe: Type): RuntimeClass

    /** Maps a Scala class symbol to the corresponding Java class object
     *  @throws java.lang.ClassNotFoundException if there is no Java class
     *          corresponding to the given Scala class symbol.
     *  Note: If the Scala symbol is ArrayClass, a ClassNotFound exception is thrown
     *        because there is no unique Java class corresponding to a Scala generic array
     */
    def runtimeClass(cls: ClassSymbol): RuntimeClass

    /** A class symbol for the specified runtime class.
     *  @return The class symbol for the runtime class in the current class loader.
     *  @throws java.lang.ClassNotFoundException if no class with that name exists
     *  @throws scala.ScalaReflectionException if no corresponding symbol exists
     *  to do: throws anything else?
     */
    def classSymbol(rtcls: RuntimeClass): ClassSymbol

    /** A module symbol for the specified runtime class.
     *  @return The module symbol for the runtime class in the current class loader.
     *  @throws java.lang.ClassNotFoundException if no class with that name exists
     *  @throws scala.ScalaReflectionException if no corresponding symbol exists
     *  to do: throws anything else?
     */
    def moduleSymbol(rtcls: RuntimeClass): ModuleSymbol
  }
}
