import scala.reflect.{ClassTag, classTag}
import java.lang.Integer

trait Gen[A] extends Any {
  def x: A
  def plus(x1: Gen[A], x2: Gen[A]): Gen[A]
}
class ValueInt(val x: Int) extends AnyVal with Gen[Int] {
  // Gen<java.lang.Object> ValueInt.extension$plus(int,Gen<java.lang.Object>,Gen<java.lang.Object>)
  def plus(x1: Gen[Int], x2: Gen[Int]): Gen[Int] = new ValueInt(x + x1.x + x2.x)
  // int ValueInt.extension$iplus(int,int,int)
  def iplus(x1: ValueInt, x2: ValueInt): ValueInt = new ValueInt(x + x1.x + x2.x)
}
class RefInt(val x: Int) extends AnyRef with Gen[Int] {
  def plus(x1: Gen[Int], x2: Gen[Int]): Gen[Int] = new RefInt(x + x1.x + x2.x)
  def rplus(x1: RefInt, x2: RefInt): RefInt = new RefInt(x + x1.x + x2.x)
}
class RefInteger(val x: java.lang.Integer) extends AnyRef with Gen[Integer] {
  def plus(x1: Gen[Integer], x2: Gen[Integer]): Gen[Integer] = new RefInteger(x + x1.x + x2.x)
  def bplus(x1: RefInteger, x2: RefInteger): RefInteger = new RefInteger(x + x1.x + x2.x)
}

class Val[Q](val value: Int) extends AnyVal
class ValAny[Q](val value: Any) extends AnyVal
class ValStr[Q](val value: String) extends AnyVal
class ValA[Q](val value: Q) extends AnyVal {
  def f: Q = ???
}
class ValB[Q, Q0 <: Q](val value: Q) extends AnyVal {
  def f: Q0 = ???
}

class C0 {
  def v1[A](in: Val[A]) = in
  def v2[A]: List[Val[A]] = Nil
  def v3[A]: Val[A] = new Val[A](0)
  def v4[A <: String](x: Val[A], ys: List[Val[A]]) = ys.head
}
class C1 {
  def v1[A](in: ValAny[A]) = in
  def v2[A]: List[ValAny[A]] = Nil
  def v3[A]: ValAny[A] = new ValAny[A]("")
  def v4[A <: String](x: ValAny[A], ys: List[ValAny[A]]) = ys.head
}
class C2 {
  def v1[A](in: ValStr[A]) = in
  def v2[A]: List[ValStr[A]] = Nil
  def v3[A]: ValStr[A] = new ValStr[A]("")
  def v4[A <: String](x: ValStr[A], ys: List[ValStr[A]]) = ys.head
}
class C3[A](val x: A) {
  def v1(in: ValA[A]) = in
  def v2: List[ValA[A]] = Nil
  def v3: ValA[A] = new ValA[A](x)
  def v4(x: ValA[A], ys: List[ValA[A]]) = ys.head
}
class C4 {
  def v1(in: ValA[Int]) = in
  def v2: List[ValA[Int]] = Nil
  def v3: ValA[Int] = new ValA(1)
  def v4(x: ValA[Int], ys: List[ValA[Int]]) = ys.head
}
class C4B {
  def v1(in: ValA[String]) = in
  def v2: List[ValA[String]] = Nil
  def v3: ValA[String] = new ValA("")
  def v4(x: ValA[String], ys: List[ValA[String]]) = ys.head
}
class C5 {
  def f1[A](x1: Val[A], x2: ValAny[A], x3: ValStr[A], x4: ValA[A]) = x4
  def f2(x1: Int, x2: Any, x3: String, x4: Double) = x4
  def f3(x: ValA[Int]) = x.f
  def f4(x: ValB[Int, Int]) = x.f
  def f5(x: ValB[Int, _ <: Int]) = x.f
}
class C6[A] {
  def f1(x1: Val[A], x2: ValAny[A], x3: ValStr[A], x4: ValA[A]) = x4
}
class C7 extends C6[Int] {
  override def f1(x1: Val[Int], x2: ValAny[Int], x3: ValStr[Int], x4: ValA[Int]) =
    super.f1(x1, x2, x3, x4)
}

object Test {
  def show[A: ClassTag] = {
    println(classTag[A].runtimeClass.getName)
    classTag[A].runtimeClass.getDeclaredMethods.toList.sortBy(_.toString).flatMap(m => List(m.toString, m.toGenericString)) foreach println
    println("")
  }

  def main(args: Array[String]): Unit = {
    show[C0]
    show[C1]
    show[C2]
    show[C3[_]]
    show[C4]
    show[C4B]
    show[C5]
    show[C6[_]]
    show[C7]
    show[Gen[_]]
    show[ValueInt]
    show[RefInt]
    show[RefInteger]
  }
}
