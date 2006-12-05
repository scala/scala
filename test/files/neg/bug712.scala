trait A {
  type Node <: NodeImpl;
  implicit def coerce(n : NodeImpl) = n.self;
  trait NodeImpl {
    def self : Node;
  }
}
trait B extends A {
  type Parent <: ParentImpl;
  implicit def coerce(p : ParentImpl) = p.self;
  trait ParentImpl;
  type Symbol;
  trait SymbolImpl {
    def scope : Int;
  }
  implicit def coerceSym(sym : Symbol) : SymbolImpl;
  var s : Symbol = _;
  val s_scope = s.scope;
}
