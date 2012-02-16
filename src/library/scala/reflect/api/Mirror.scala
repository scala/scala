package scala.reflect
package api

/** A mirror establishes connections of
 *  runtime entities such as class names and object instances
 *  with a reflexive universe.
 */
trait Mirror extends Universe with RuntimeTypes with TreeBuildUtil {

  /** The Scala class symbol that has given fully qualified name
   *  @param name  The fully qualified name of the class to be returned
   *  @throws java.lang.ClassNotFoundException if no class with that name exists
   *  to do: throws anything else?
   */
  def symbolForName(name: String): Symbol
  
  /** Return a reference to the companion object of the given class symbol.
   */
  def companionInstance(clazz: Symbol): AnyRef
  
  /** The Scala class symbol corresponding to the runtime class of the given instance.
   *  @param    instance    The instance
   *  @return               The class Symbol for the instance
   *  @throws ?
   */
  def symbolOfInstance(instance: Any): Symbol

  /** The Scala type corresponding to the runtime type of given instance.
   *  If the underlying class is parameterized, this will be an existential type,
   *  with unknown type arguments.
   *
   *  @param    instance    The instance.
   *  @return               The Type of the given instance.
   *  @throws ?
   */
  def typeOfInstance(instance: Any): Type

  /** The value of a field on a receiver instance.
   *  @param receiver   The receiver instance
   *  @param field      The field
   *  @return           The value contained in `receiver.field`.
   */
  def getValueOfField(receiver: AnyRef, field: Symbol): Any

  /** Sets the value of a field on a receiver instance.
   *  @param receiver   The receiver instance
   *  @param field      The field
   *  @param value      The new value to be stored in the field.
   */
  def setValueOfField(receiver: AnyRef, field: Symbol, value: Any): Unit

  /** Invokes a method on a receiver instance with some arguments
   *  @param receiver   The receiver instance
   *  @param meth       The method
   *  @param args       The method call's arguments
   *  @return   The result of invoking `receiver.meth(args)`
   */
  def invoke(receiver: AnyRef, meth: Symbol)(args: Any*): Any

  /** Maps a Java class to a Scala type reference
   *  @param   clazz    The Java class object
   *  @return  A type (of kind `TypeRef`, or `ExistentialType` if `clazz` is polymorphic)
   *           that represents the class with all type parameters unknown
   *           (i.e. any type parameters of `clazz` are existentially quantified).
   *  */
  def classToType(clazz: java.lang.Class[_]): Type

  /** Maps a Java class to a Scala class symbol
   *  @param   clazz    The Java class object
   *  @return  A symbol that represents the Scala view of the class.
   */
  def classToSymbol(clazz: java.lang.Class[_]): Symbol

  /** Maps a Scala type to the corresponding Java class object
   */
  def typeToClass(tpe: Type): java.lang.Class[_]

  /** Maps a Scala symbol to the corresponding Java class object
   *  @throws ClassNotFoundException if there is no Java class
   *          corresponding to the given Scala symbol.
   *  Note: If the Scala symbol is ArrayClass, a ClassNotFound exception is thrown
   *        because there is no unique Java class corresponding to a Scala generic array
   */
  def symbolToClass(sym: Symbol): java.lang.Class[_]
}
