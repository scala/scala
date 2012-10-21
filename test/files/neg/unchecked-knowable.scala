/** Knowable - only final leaves */
sealed abstract class A1
sealed abstract class A2 extends A1
final class A3 extends A1
final class A4 extends A2

/** Unknowable */
sealed abstract class B1
sealed abstract class B2 extends B1
sealed trait B2B extends B1
final class B3 extends B1
trait B4 extends B2

class Bippy
trait Dingus

class A {
  /*   warn */ (new Bippy).isInstanceOf[A1]
  /*   warn */ (new Bippy).isInstanceOf[B1]
  /* nowarn */ (null: Dingus).isInstanceOf[B1]
  /* nowarn */ ((new Bippy): Any).isInstanceOf[A1]
}
