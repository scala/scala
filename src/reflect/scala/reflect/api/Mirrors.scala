package scala.reflect
package api

trait Mirrors { self: Universe =>

  type RuntimeClass >: Null

  // [Eugene] also, it might make sense to provide shortcuts for the API
  //
  // for example, right now to invoke the same method for several different instances, you need:
  // 1) get the method symbol
  // 2) get the instance mirror for every instance
  // 3) call reflectMethod on the instance mirrors for every instance
  // 4) call apply for every instance (okay, this can be united with step #3, but still)
  //
  // I have several suggestions that we can discuss later:
  // 1) For every `reflectXXX(sym: Symbol): XXXMirror`, add `reflectXXX(name: String, types: Type*): XXXMirror` and `reflectXXXs(): List[XXXMirror]`
  // 2) Provide a way to skip obtaining InstanceMirror (step #2 in the outline provided above)

  // [Eugene] another improvement would be have mirrors reproduce the structure of the reflection domain
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
     *  To get a field symbol by the name of the field you would like to reflect,
     *  use `<this mirror>.symbol.typeSignature.member(newTermName(<name of the field>)).asTermSymbol`.
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
     *  use `<this mirror>.symbol.typeSignature.member(newTermName(<name of the method>)).asMethodSymbol`.
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
     *  use `<this mirror>.symbol.typeSignature.member(newTypeName(<name of the class>)).asClassSymbol`.
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
     *  use `<this mirror>.symbol.typeSignature.member(newTermName(<name of the object>)).asModuleSymbol`.
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
    def receiver: AnyRef

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
     */
    def get: Any

    /** Updates the value stored in the field.
     *
     *  If a field is immutable, a `ScalaReflectionException` will be thrown.
     *
     *  Scala reflection uses reflection capabilities of the underlying platform,
     *  so `FieldMirror.get` might throw platform-specific exceptions associated
     *  with setting a field or invoking a setter method of the field.
     */
    def set(value: Any): Unit
  }

  /** A mirror that reflects a method handle */
  trait MethodMirror {

    /** The receiver object of the method */
    def receiver: AnyRef

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
     *  if a runtime class or the companion object of a Scala class.
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
    def companion: Option[ClassMirror]
  }

  /** A mirror that reflects the instance parts of a runtime class */
  trait ClassMirror extends TemplateMirror {

    /** The Scala class symbol corresponding to the reflected class */
    override def symbol: ClassSymbol

    /** Reflects against a constructor symbol and returns a mirror
     *  that can be used to invoke it and construct instances of this mirror's symbols.
     *
     *  To get a constructor symbol you would like to reflect,
     *  use `<this mirror>.symbol.typeSignature.member(nme.CONSTRUCTOR).asMethodSymbol`.
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
    def companion: Option[ModuleMirror]
  }

  /** A mirror that reflects instances and static classes */
  trait ReflectiveMirror extends MirrorOf[Mirrors.this.type] {

    /** A reflective mirror for the given object.
     *
     *  Such a mirror can be used to further reflect against the members of the object
     *  to get/set fields, invoke methods and inspect inner classes and objects.
     */
    def reflect(obj: Any): InstanceMirror

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
