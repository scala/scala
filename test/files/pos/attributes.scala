/* $Id$ */

[serializable] class C1;
[serializable,volatile] class C2;
[serializable][volatile] class C3;

[serializable] trait T1;
[serializable,volatile] trait T2;
[serializable][volatile] trait T3;

[serializable] object O1 extends C1;
[serializable,volatile] object O2 extends C2;
[serializable][volatile] object O3 extends C3;

object O4 {
  final val n = 2;
  [SerialVersionUID(0)] class C1;
  [SerialVersionUID(n)] class C2;
}

abstract class A1 {
  [serializable] var y1: C1;
  [serializable,volatile] var y2: C2;
  [serializable][volatile] var y3: C3;

  [serializable] def foo1: C1;
  [serializable,volatile] def foo2: C2;
  [serializable][volatile] def foo3: C3;
}

object O5 {
  [serializable] val x1 = new C1;
  [serializable,volatile] val x2 = new C2;
  [serializable][volatile] val x3 = new C3;

  [serializable] var y1: C1 = _;
  [serializable,volatile] var y2: C2 = _;
  [serializable][volatile] var y3: C3 = _;

  [serializable] private def foo1 = x1;
  [serializable,volatile] private def foo2 = x2;
  [serializable][volatile] protected def foo3 = x3;
}

object myAttributes {
  class a1 extends scala.Attribute;
}
object O6 {
  class a2 extends scala.Attribute;
  [myAttributes.a1] class C1;
  [myAttributes.a1,a2] class C2;
  [myAttributes.a1][a2] class C3;

}
