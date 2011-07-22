package scala.reflect
package api

/** A mirror establishes connections of
 *  runtime entities such as class names and object instances
 *  with a refexive universe.
 */
trait Mirror extends Universe {

  /** The Scala class symbol that has given fully qualified name
   *  @param name  The fully qualified name of the class to be returned
   *  @throws java.lang.ClassNotFoundException if no class wiht that name exists
   *  to do: throws anything else?
   */
  def classWithName(name: String): Symbol

  /** The Scala class symbol corresponding to the runtime class of given object
   *  @param  The object from which the class is returned
   *  @throws ?
   */
  def getClass(obj: AnyRef): Symbol

  /** The Scala type corresponding to the runtime type of given object.
   *  If the underlying class is parameterized, this will be an existential type,
   *  with unknown type arguments.
   *
   *  @param  The object from which the type is returned
   *  @throws ?
   */
  def getType(obj: AnyRef): Type

  def getValue(receiver: AnyRef, field: Symbol): Any
  def setValue(receiver: AnyRef, field: Symbol, value: Any): Unit
  def invoke(receiver: AnyRef, meth: Symbol, args: Any*): Any
}