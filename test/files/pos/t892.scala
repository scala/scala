package test;
object Test {
  trait Core {
    abstract class Visitor[T <: Visitor[T]];
    trait HasVisitor {
      def visit[T <: Visitor[T]](initial : T) : T;
    }
  }
  trait Ext extends Core {
    class Foo {
      def visit[T <: Visitor[T]](initial : T) : T = initial;
    }
  }
}
