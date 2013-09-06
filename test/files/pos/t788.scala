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
// t788.scala:16:
//     case f : NamedImpl => f.self;
//              ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Test.this.ExpressionImpl
//         pt  Test.this.ExpressionImpl
//      pattp  Test.this.NamedImpl
//   pattp+pt  Test.this.NamedImpl with Test.this.ExpressionImpl
//   pt+pattp  Test.this.ExpressionImpl with Test.this.NamedImpl
//     result  Test.this.ExpressionImpl with Test.this.NamedImpl
//
//        pt0 =:= pt             pt0 !:= pattp     pattp+pt <:< pt0       pt+pattp <:< pt0         result <:< pt0
//         pt !:= pattp     pattp+pt <:< pt        pt+pattp <:< pt          result <:< pt
//   pattp+pt <:< pattp     pt+pattp <:< pattp       result <:< pattp
//   pattp+pt ~:= pt+pattp  pattp+pt ~:= result
//   pt+pattp =:= result
//
// }