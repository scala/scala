package test;

trait Test {
  type Node <: NodeImpl;
  trait NodeImpl;
  type Expression <: Node with ExpressionImpl;
  trait ExpressionImpl extends NodeImpl {
    def self : Expression;
  }
  type Named <: Node with NamedImpl;
  trait NamedImpl extends NodeImpl {
    def self : Named;
  }
  def asExpression(e : ExpressionImpl) : Named = {
    e match {
    case f : NamedImpl => f.self;
    }
  }
}
