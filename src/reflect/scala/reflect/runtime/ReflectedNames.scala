package scala.reflect
package runtime

/** Decorates selected methods of names that belong to runtime reflection universes
 *  with synchronization facilities.
 */
trait ReflectedNames extends internal.Names { self: SymbolTable =>

  private lazy val nameLock = new Object

  override def newTermName(s: String): TermName = nameLock.synchronized { super.newTermName(s) }
  override def newTypeName(s: String): TypeName = nameLock.synchronized { super.newTypeName(s) }
}
