package scala.reflect
package api

trait Mirrors extends base.Mirrors { self: Universe =>

  type RuntimeClass >: Null

  // todo. an improvement might be having mirrors reproduce the structure of the reflection domain
  // e.g. a ClassMirror could also have a list of fields, methods, constructors and so on
  // read up more on the proposed design in "Reflecting Scala" by Y. Coppel

  /** A mirror that reflects a runtime value */
  trait InstanceMirror {

    /** The instance value reflected by this mirror */
    def instance: Any

    /** The symbol corresponding to the run-time class of the reflected instance */
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
     *  use `<this mirror>.symbol.typeSignature.member(newTermName(<name of the field>)).asTerm.accessed`.
     *  For further information about member lookup refer to `Symbol.typeSignature`.
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
     *  use `<this mirror>.symbol.typeSignature.member(newTermName(<name of the method>)).asMethod`.
     *  For further information about member lookup refer to `Symbol.typeSignature`.
     *
     *  The input symbol can be either private or non-private (Scala reflection transparently deals with visibility).
     *  It must be a member (declared or inherited) of the instance underlying this mirror.
     */
    def reflectMethod(method: MethodSymbol): MethodMirror

    /** Reflects against an inner class symbol and returns a mirror
     *  that can be used to create instances of the class, inspect its companion object or perform further reflections.
     *
     *  To get a class symbol by the name of the class you would like to reflect,
     *  use `<this mirror>.symbol.typeSignature.member(newTypeName(<name of the class>)).asClass`.
     *  For further information about member lookup refer to `Symbol.typeSignature`.
     *
     *  The input symbol can be either private or non-private (Scala reflection transparently deals with visibility).
     *  It must be a member (declared or inherited) of the instance underlying this mirror.
     */
    def reflectClass(cls: ClassSymbol): ClassMirror

    /** Reflects against an inner module symbol and returns a mirror
     *  that can be used to get the instance of the object or inspect its companion class.
     *
     *  To get a module symbol by the name of the object you would like to reflect,
     *  use `<this mirror>.symbol.typeSignature.member(newTermName(<name of the object>)).asModule`.
     *  For further information about member lookup refer to `Symbol.typeSignature`.
     *
     *  The input symbol can be either private or non-private (Scala reflection transparently deals with visibility).
     *  It must be a member (declared or inherited) of the instance underlying this mirror.
     */
    def reflectModule(mod: ModuleSymbol): ModuleMirror
  }

  /** A mirror that reflects a field */
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
     *  and this base field is overriden in the class of the receiver, then this method will retrieve
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
     *  and this base field is overriden in the class of the receiver, then this method will set
     *  the value of the base field. To achieve overriding behavior, use reflectMethod on an accessor.
     */
    def set(value: Any): Unit
  }

  /** A mirror that reflects a method handle */
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
  }

  /** A mirror that reflects the instance or static parts of a runtime class */
  trait TemplateMirror {

    /** The runtime class reflected by this mirror */
    def runtimeClass: RuntimeClass

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

    /** Optionally, the mirror of the companion reflected by this mirror.
     *  If this mirror reflects a Scala object, the mirror for the companion class, or None
     *  if the mirror represents a Scala object that comes without a class.
     *  Otherwise, if the mirror represents the static part of a runtime class, the
     *  mirror representing the instance part of the same class.
     *  Otherwise, if the mirror represents a Scala instance class, the mirror for the companion
     *  object of that class, or None if no such object exists.
     *  Otherwise, if the mirror represents a runtime instance class, a mirror representing the static
     *  part of the same class.
     */
    def companion: Option[TemplateMirror]
  }

  /** A mirror that reflects a Scala object definition or the static parts of a runtime class */
  trait ModuleMirror extends TemplateMirror {

    /** The Scala module symbol corresponding to the reflected object */
    override def symbol: ModuleSymbol

    /** If the reflected runtime class corresponds to a Scala object definition,
     *  returns the single instance representing that object.
     *  If this mirror reflects the static part of a runtime class, returns `null`.
     */
    def instance: Any

    /** Optionally, the mirror of the companion class if the object reflected by this mirror.
     *  If this mirror reflects a Scala object, the mirror for the companion class, or None
     *  if the mirror represents a Scala object that comes without a class.
     *  Otherwise, if the mirror represents the static part of a runtime class, the
     *  mirror representing the instance part of the same class.
     */
    override def companion: Option[ClassMirror]
  }

  /** A mirror that reflects the instance parts of a runtime class */
  trait ClassMirror extends TemplateMirror {

    /** The Scala class symbol corresponding to the reflected class */
    override def symbol: ClassSymbol

    /** Reflects against a constructor symbol and returns a mirror
     *  that can be used to invoke it and construct instances of this mirror's symbols.
     *
     *  To get a constructor symbol you would like to reflect,
     *  use `<this mirror>.symbol.typeSignature.member(nme.CONSTRUCTOR).asMethod`.
     *  For further information about member lookup refer to `Symbol.typeSignature`.
     *
     *  The input symbol can be either private or non-private (Scala reflection transparently deals with visibility).
     *  It must be a member (declared or inherited) of the class underlying this mirror.
     */
    def reflectConstructor(constructor: MethodSymbol): MethodMirror

    /** Optionally, the mirror of the companion object of the class reflected by this mirror.
     *  If this mirror represents a Scala instance class, the mirror for the companion
     *  object of that class, or None if no such object exists.
     *  Otherwise, if the mirror represents a runtime instance class, a mirror representing the static
     *  part of the same class.
     */
    override def companion: Option[ModuleMirror]
  }

  /** A mirror that reflects instances and static classes */
  trait ReflectiveMirror extends MirrorOf[Mirrors.this.type] {

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

  /** The API of a mirror for a reflective universe */
  trait RuntimeMirror extends ReflectiveMirror { self =>

    /** Maps a Scala type to the corresponding Java class object */
    def runtimeClass(tpe: Type): RuntimeClass

    /** Maps a Scala class symbol to the corresponding Java class object
     *  @throws ClassNotFoundException if there is no Java class
     *          corresponding to the given Scala class symbol.
     *  Note: If the Scala symbol is ArrayClass, a ClassNotFound exception is thrown
     *        because there is no unique Java class corresponding to a Scala generic array
     */
    def runtimeClass(cls: ClassSymbol): RuntimeClass

    /** A class symbol for the specified runtime class.
     *  @return The class symbol for the runtime class in the current class loader.
     *  @throws java.lang.ClassNotFoundException if no class with that name exists
     *  @throws scala.reflect.internal.MissingRequirementError if no corresponding symbol exists
     *  to do: throws anything else?
     */
    def classSymbol(rtcls: RuntimeClass): ClassSymbol

    /** A module symbol for the specified runtime class.
     *  @return The module symbol for the runtime class in the current class loader.
     *  @throws java.lang.ClassNotFoundException if no class with that name exists
     *  @throws scala.reflect.internal.MissingRequirementError if no corresponding symbol exists
     *  to do: throws anything else?
     */
    def moduleSymbol(rtcls: RuntimeClass): ModuleSymbol
  }
}
