package mixins;
abstract class Super {
  def foo: int;
}
abstract class Mixin extends Super {
  abstract override def foo = super.foo;
}
class Sub extends Super with Mixin {
  override def foo: int = 1;
}
abstract class Mixin1 extends Mixin {
  abstract override def foo = super.foo;
}
abstract class Base0 extends Super with Mixin {
}
class Base extends Sub with Mixin1 {
}
