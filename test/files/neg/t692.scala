abstract class test3 {
  trait Type0[+T0];
  trait Type[T0] extends Type0[T];
  trait ClassType0[+C <: AnyRef] extends Type0[C];
  abstract class RefType[C <: AnyRef] extends Type[C];
  case class ObjectType() extends RefType[AnyRef];
  abstract class ClassType[C <: Z, Z <: AnyRef](zuper : RefType[Z]) extends RefType[C];


  case class FooType() extends ClassType[Foo,AnyRef](ObjectType());
  implicit def typeOfFoo = FooType();

  case class BarType[T3 <: Foo](tpeT : RefType[T3]) extends ClassType[Bar[T3],Foo](FooType);
  implicit def typeOfBar[T4 <: Foo](implicit elem : RefType[T4]) : RefType[Bar[T4]] =
    BarType(elem);


  class Foo[A <: AnyRef];
  class Bar[A <: Foo](implicit tpeA : Type[A]) extends Foo;
}
