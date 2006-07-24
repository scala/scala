package test;
abstract class Test;
trait Test2 extends Test {
  trait Foo extends super.Foo {
    trait Bar extends super.Bar;
  }
}
