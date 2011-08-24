package test;

abstract class XXX;

trait YYY extends XXX {
  val y = 10;
}

class Foo extends XXX with YYY {
  override val y = super.y;
}
