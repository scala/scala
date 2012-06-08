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

    /** The symbol corresponding to the run-time class of the reflected instance. */
    def symbol: ClassSymbol

    /** Get value of field in reflected instance.
     *  @field  A field symbol that should represent a field of the instance class.
     *  @return The value associated with that field in the reflected instance
     *  @throws ???
     */
    def reflectField(field: TermSymbol): FieldMirror

    /** Invokes a method on the reflected instance.
     *  @param meth A method symbol that should represent a method of the instance class
     *  @param args The arguments to pass to the method
     *  @return   The result of invoking `meth(args)` on the reflected instance.
     *  @throws ???
     */
    def reflectMethod(method: MethodSymbol): MethodMirror

    /** .. */
    def reflectClass(cls: ClassSymbol): ClassMirror

    /** .. */
    def reflectModule(mod: ModuleSymbol): ModuleMirror
  }

  /** A mirror that reflects a field */
  trait FieldMirror {

    /** The object containing the field */
    def receiver: AnyRef

    /** The field symbol representing the field */
    def symbol: TermSymbol

    /** Retrieves the value stored in the field */
    def get: Any

    /** Updates  the value stored in the field */
    def set(value: Any): Unit
  }

  /** A mirror that reflects a method handle */
  trait MethodMirror {

    /** The receiver object of the method */
    def receiver: AnyRef

    /** The method symbol representing the method */
    def symbol: MethodSymbol

    /** The result of applying the method to the given arguments */
    // [Eugene+++] If it's a constructor, it should account for inner classes
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

    /** The Scala symbol corresponding to the reflected runtime class or module. */
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

    /** The Scala module symbol corresponding to the reflected module. */
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

    /** The Scala class symbol corresponding to the reflected class. */
    override def symbol: ClassSymbol

    /** Returns a fresh instance of by invoking that constructor.
     *  @throws InstantiationException   if the class does not have a public
     *                                   constructor with an empty parameter list.
     *  @throws IllegalAccessException  if the class or its constructor is not accessible.
     *  @throws ExceptionInInitializerError if the initialization of the constructor fails.
     *  @throws SecurityException    if creating a new instance is not permitted.
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

    /** A reflective mirror for the given object
     *  @param  obj   An arbitrary value
     *  @return The mirror for `obj`.
     */
    def reflect(obj: Any): InstanceMirror

    /** .. */
    def reflectClass(cls: ClassSymbol): ClassMirror

    /** .. */
    def reflectModule(mod: ModuleSymbol): ModuleMirror
  }

  /** The API of a mirror for a reflective universe */
  trait RuntimeMirror extends ReflectiveMirror { self =>

    /** Maps a Scala type to the corresponding Java class object
     */
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
     *  @return       The module symbol for the runtime class in the current class loader.
     *  @throws java.lang.ClassNotFoundException if no class with that name exists
     *  @throws scala.reflect.internal.MissingRequirementError if no corresponding symbol exists
     *  to do: throws anything else?
     */
    def moduleSymbol(rtcls: RuntimeClass): ModuleSymbol
  }
}
