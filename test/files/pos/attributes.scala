@serializable  class C1;
@serializable @volatile  class C2;
@serializable @volatile  class C3;
@serializable @volatile @serializable class C4;

@serializable  trait T1;
@serializable @volatile trait T2;
@serializable @volatile  trait T3;
@serializable @volatile @serializable trait T4;

@serializable  object O1 extends C1;
@serializable @volatile object O2 extends C2;
@serializable @volatile  object O3 extends C3;
@serializable @volatile @serializable object O4 extends C4;

object O5 {
  final val n = 2;
  @SerialVersionUID(0)  class C1;
  @SerialVersionUID(n)  class C2;
  @SerialVersionUID(0) @SerialVersionUID(n)  class C3;
  @SerialVersionUID(0) @SerialVersionUID(n)  class C4;
}

abstract class A1 {
  @serializable  var y1: C1;
  @serializable @volatile  var y2: C2;
  @serializable @volatile  var y3: C3;
  @serializable @volatile @serializable  var y4: C4;

  @serializable  def foo1: C1;
  @serializable @volatile  def foo2: C2;
  @serializable @volatile  def foo3: C3;
  @serializable @volatile @serializable  def foo4: C4;
}

object O6 {
  @serializable  val x1 = new C1;
  // volatile val sensibly disallowed as of r18645
  @serializable val x2 = new C2;
  @serializable val x3 = new C3;
  @serializable val x4 = new C4;

  @serializable  var y1: C1 = _;
  @serializable @volatile  var y2: C2 = _;
  @serializable @volatile  var y3: C3 = _;
  @serializable @volatile @serializable  var y4: C4 = _;

  @serializable  private def foo1 = x1;
  @serializable @volatile  private def foo2 = x2;
  @serializable @volatile  protected def foo3 = x3;
  @serializable @volatile @serializable  protected def foo4 = x4;
}

object myAttrs {
  class a1 extends scala.Annotation;
  class a2(x: Int) extends scala.Annotation;
  class a3(x: a1) extends scala.Annotation;
}
class a4(ns: Array[Int]) extends scala.Annotation;
object O7 {
  class a1 extends scala.Annotation;
  class a2(x: Int) extends scala.Annotation;
  class a3(x: a1) extends scala.Annotation;

  final val x = new a1;
  @a1  class C1;
  @a1 @a2(77)  class C2;
  @a1 @a2(88)  class C3;
  @a1 @a2(88) @a3(null)  class C4;

  @myAttrs.a1  class A1;
  @myAttrs.a1 @myAttrs.a2(99)  class A2;
  @myAttrs.a1 @myAttrs.a2(99)  class A3;
  @myAttrs.a1 @myAttrs.a2(99) @myAttrs.a3(null)  class A4;
  @a4(Array(1,2,3)) class A5;
  @a4(Array()) class A6;

  val zero = 0 : @myAttrs.a1
}
