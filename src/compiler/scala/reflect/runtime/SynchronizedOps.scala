package scala.reflect
package runtime

trait SynchronizedOps extends internal.SymbolTable 
                         with SynchronizedSymbols
                         with SynchronizedTypes { self: SymbolTable =>
                           
// Names
                           
  private lazy val nameLock = new Object
  
  override def newTermName(s: String): TermName = nameLock.synchronized { super.newTermName(s) }
  override def newTypeName(s: String): TypeName = nameLock.synchronized { super.newTypeName(s) }
  
// BaseTypeSeqs
 
  override protected def newBaseTypeSeq(parents: List[Type], elems: Array[Type]) = 
    new BaseTypeSeq(parents, elems) with SynchronizedBaseTypeSeq
    
  trait SynchronizedBaseTypeSeq extends BaseTypeSeq {
    override def apply(i: Int): Type = synchronized { super.apply(i) }
    override def rawElem(i: Int) = synchronized { super.rawElem(i) }
    override def typeSymbol(i: Int): Symbol = synchronized { super.typeSymbol(i) }
    override def toList: List[Type] = synchronized { super.toList }
    override def copy(head: Type, offset: Int): BaseTypeSeq = synchronized { super.copy(head, offset) }
    override def map(f: Type => Type): BaseTypeSeq = synchronized { super.map(f) }
    override def exists(p: Type => Boolean): Boolean = synchronized { super.exists(p) }
    override lazy val maxDepth = synchronized { maxDepthOfElems }
    override def toString = synchronized { super.toString }

    override def lateMap(f: Type => Type): BaseTypeSeq = new MappedBaseTypeSeq(this, f) with SynchronizedBaseTypeSeq
  }
  
// Scopes
  
  override def newScope = new Scope() with SynchronizedScope
  override def newNestedScope(outer: Scope): Scope = new Scope(outer) with SynchronizedScope

  trait SynchronizedScope extends Scope {
    override def isEmpty: Boolean = synchronized { super.isEmpty }
    override def size: Int = synchronized { super.size }
    override def enter(sym: Symbol) = synchronized { super.enter(sym) }
    override def rehash(sym: Symbol, newname: Name) = synchronized { super.rehash(sym, newname) }
    override def unlink(e: ScopeEntry) = synchronized { super.unlink(e) }
    override def unlink(sym: Symbol) = synchronized { super.unlink(sym) }
    override def lookupAll(name: Name) = synchronized { super.lookupAll(name) }
    override def lookupEntry(name: Name) = synchronized { super.lookupEntry(name) }
    override def lookupNextEntry(entry: ScopeEntry) = synchronized { super.lookupNextEntry(entry) }
    override def toList: List[Symbol] = synchronized { super.toList }
  }
}
