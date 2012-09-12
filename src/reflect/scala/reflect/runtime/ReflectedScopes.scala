package scala.reflect
package runtime

/** Decorates selected methods of scopes that belong to runtime reflection universes
 *  with synchronization facilities.
 */
trait ReflectedScopes extends internal.Scopes { self: SymbolTable =>

  override def newScope = new Scope() with SynchronizedScope
  override def newNestedScope(outer: Scope): Scope = new Scope(outer) with SynchronizedScope

  trait SynchronizedScope extends Scope {
    override def isEmpty: Boolean = synchronized { super.isEmpty }
    override def size: Int = synchronized { super.size }
    override def enter[T <: Symbol](sym: T): T = synchronized { super.enter(sym) }
    override def rehash(sym: Symbol, newname: Name) = synchronized { super.rehash(sym, newname) }
    override def unlink(e: ScopeEntry) = synchronized { super.unlink(e) }
    override def unlink(sym: Symbol) = synchronized { super.unlink(sym) }
    override def lookupAll(name: Name) = synchronized { super.lookupAll(name) }
    override def lookupEntry(name: Name) = synchronized { super.lookupEntry(name) }
    override def lookupNextEntry(entry: ScopeEntry) = synchronized { super.lookupNextEntry(entry) }
    override def toList: List[Symbol] = synchronized { super.toList }
  }
}
