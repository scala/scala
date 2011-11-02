//############################################################################
// Type Representation at runtime
//############################################################################

import TypeRep._

object Test extends App {
  testPrimitives
  testOptions
  testLists
  testArrays
  testTuples
  testFuncs
  testClasses
}

object serialize {
  import java.io._

  @throws(classOf[IOException])
  def write[A](o: A): Array[Byte] = {
    val ba = new ByteArrayOutputStream(512)
    val out = new ObjectOutputStream(ba)
    out.writeObject(o)
    out.close()
    ba.toByteArray()
  }
  @throws(classOf[IOException])
  @throws(classOf[ClassNotFoundException])
  def read[A](buffer: Array[Byte]): A = {
    val in =
      new ObjectInputStream(new ByteArrayInputStream(buffer))
    in.readObject().asInstanceOf[A]
  }
}

object testPrimitives {
  println(getType(true))
  val b = false; val bt = getType(b)
  println(bt)
  val bt1: TypeRep[Boolean] = serialize.read(serialize.write(bt))
  println(bt1 == bt)
  println(getType(16.toByte))
  println(getType('a'))
  println(getType(3))
  println(getType(3l))
  println(getType(0.0f))
  println(getType(0.0d))
  println(getType("abc"))
  println(getType(())) // Unit
  println(getType(classOf[Int])) // Class
  println
}

object testOptions {
  println(getType(Some(2)))
  val x: Option[Int] = Some(2)
  println(getType(x))
  println(getType(Some(Some(3))))
  println(getType(Some(List(3))))
  //println(getType(Some(None: List[Int])))  // error: no implicit argument matching parameter type TypeRep[object None] was foun
  println(getType(None: Option[Int]))
  val y: Option[Int] = None
  println(getType(y))
  println
}

object testLists {
  println(getType(List(3)))
  println(getType(3 :: Nil))
  println(getType(List(List(3))))
  println(getType(Nil: List[Int]))
  println(getType(List(1, "abc")))
  println
}

object testArrays {
  println(getType(Array(3)))
  println(getType(Array(Array(3), Array(4))))
  println(getType(new Array[Int](0)))
  println(getType(List(1).toArray))
  println(getType(List[Int]().toArray))
  println(getType(Array(3).drop(1).toArray)) // empty
  println
}

object testTuples {
  println(getType((3, "abc")))
  println(getType(Triple('a', 'b', "c")))
  println(getType(((3, "abc"), (4, "xyz"))))
  println(getType(((Some('b'), 3), (Some('a'), 4))))
  //println(getType(((Some('b'), 3), (None, 4))))
  println
}

object testFuncs {
  def f1(x: Int): Int = 2 * x
  println(getType(f1 _))
  println(getType(f1(2)))
  val f2 = (x: Int) => 2 * x
  println(getType(f2))
  println(getType(f2(2)))
  val f3 = (x: Int) => (y: Int) => x + y
  println(getType(f3))
  println(getType(f3(2)))
  println(getType(f3(2)(2)))
  def f4(b: Boolean, c: List[Char], i: Int): Int = i
  println(getType(f4 _))
  def f5(f: Int => Int, x: Int) = f(x)
  println(getType(f5 _))
  println(getType(f5(f1, 1)))
  println  
}

class Foo {
  class Bar(x: Int)
}


object foo extends Foo

package pkg1 {
  class C1
  object c1 extends C1
}

object testClasses {
  /*
  case object FooRep extends TypeRep[Foo] {
    override def toString = "Foo"
  }
  implicit def fooRep[A](x: A)(implicit rep: TypeRep[foo.type]): TypeRep[foo.type] = rep
  println(getType(foo))
  println(getType(new foo.Bar(0)))
  val foo2 = new Foo
  println(getType(foo2))
  println(getType(new foo2.Bar(1)))
  println

  println(getType(pkg1.c1))
  val c1 = new pkg1.C1
  println(getType(c1))
  println
  */
}


/**
 *  @author Stephane Micheloud
 *  @version 1.0
 */
abstract class TypeRep[A]

object TypeRep {

  def getType[A](x: A)(implicit rep: TypeRep[A]): TypeRep[A] = rep

  def getType[A](x: Option[A])(implicit rep: TypeRep[A]): TypeRep[Option[A]] = (x match {
    case Some(v) => SomeRep(rep)
    case None    => NoneRep
  }).asInstanceOf[TypeRep[Option[A]]]

  def getType[A](x: List[A])(implicit rep: TypeRep[A]): TypeRep[List[A]] = (x match {
    case h :: t => ListRep(getType(h))
    case Nil    => NilRep
  }).asInstanceOf[TypeRep[List[A]]]

  implicit def boolRep: TypeRep[Boolean] = BooleanRep
  implicit def byteRep: TypeRep[Byte] = ByteRep
  implicit def charRep: TypeRep[Char] = CharRep
  implicit def shortRep: TypeRep[Short] = ShortRep
  implicit def intRep: TypeRep[Int] = IntRep
  implicit def longRep: TypeRep[Long] = LongRep
  implicit def floatRep: TypeRep[Float] = FloatRep
  implicit def doubleRep: TypeRep[Double] = DoubleRep

  implicit def unitRep: TypeRep[Unit] = UnitRep
  implicit def stringRep: TypeRep[String] = StringRep
  //implicit def noneRep: TypeRep[None.type] = NoneRep//[Nothing](NothingRep.asInstanceOf[TypeRep[Nothing]])
  implicit def anyRep: TypeRep[Any] = AnyRep
  implicit def nothingRep: TypeRep[Nothing] = NothingRep

  implicit def classRep[A](implicit elemrep: TypeRep[A]): TypeRep[Class[A]] =
    ClassRep(elemrep)

  implicit def someRep[A](implicit elemrep: TypeRep[A]): TypeRep[Some[A]] =
    SomeRep(elemrep)

  implicit def listRep[A](implicit elemrep: TypeRep[A]): TypeRep[List[A]] =
    ListRep(elemrep)

  implicit def arrayRep[A](implicit elemrep: TypeRep[A]): TypeRep[Array[A]] =
    ArrayRep(elemrep)

  implicit def tuple2Rep[A1, A2](implicit _1: TypeRep[A1], _2: TypeRep[A2]): TypeRep[(A1, A2)] =
    Tuple2Rep(_1, _2)

  implicit def tuple3Rep[A1, A2, A3](implicit _1: TypeRep[A1], _2: TypeRep[A2], _3: TypeRep[A3]): TypeRep[(A1, A2, A3)] =
    Tuple3Rep(_1, _2, _3)
  implicit def tuple4Rep[A1, A2, A3, A4](implicit _1: TypeRep[A1], _2: TypeRep[A2], _3: TypeRep[A3], _4: TypeRep[A4]): TypeRep[Tuple4[A1, A2, A3, A4]] =
    Tuple4Rep(_1, _2, _3, _4)
  implicit def tuple5Rep[A1, A2, A3, A4, A5](implicit _1: TypeRep[A1], _2: TypeRep[A2], _3: TypeRep[A3], _4: TypeRep[A4], _5: TypeRep[A5]): TypeRep[Tuple5[A1, A2, A3, A4, A5]] =
    Tuple5Rep(_1, _2, _3, _4, _5)
  implicit def tuple6Rep[A1, A2, A3, A4, A5, A6](implicit _1: TypeRep[A1], _2: TypeRep[A2], _3: TypeRep[A3], _4: TypeRep[A4], _5: TypeRep[A5], _6: TypeRep[A6]): TypeRep[Tuple6[A1, A2, A3, A4, A5, A6]] =
    Tuple6Rep(_1, _2, _3, _4, _5, _6)
  implicit def tuple7Rep[A1, A2, A3, A4, A5, A6, A7](implicit _1: TypeRep[A1], _2: TypeRep[A2], _3: TypeRep[A3], _4: TypeRep[A4], _5: TypeRep[A5], _6: TypeRep[A6], _7: TypeRep[A7]): TypeRep[Tuple7[A1, A2, A3, A4, A5, A6, A7]] =
    Tuple7Rep(_1, _2, _3, _4, _5, _6, _7)
  implicit def tuple8Rep[A1, A2, A3, A4, A5, A6, A7, A8](implicit _1: TypeRep[A1], _2: TypeRep[A2], _3: TypeRep[A3], _4: TypeRep[A4], _5: TypeRep[A5], _6: TypeRep[A6], _7: TypeRep[A7], _8: TypeRep[A8]): TypeRep[Tuple8[A1, A2, A3, A4, A5, A6, A7, A8]] =
    Tuple8Rep(_1, _2, _3, _4, _5, _6, _7, _8)
  implicit def tuple9Rep[A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit _1: TypeRep[A1], _2: TypeRep[A2], _3: TypeRep[A3], _4: TypeRep[A4], _5: TypeRep[A5], _6: TypeRep[A6], _7: TypeRep[A7], _8: TypeRep[A8], _9: TypeRep[A9]): TypeRep[Tuple9[A1, A2, A3, A4, A5, A6, A7, A8, A9]] =
    Tuple9Rep(_1, _2, _3, _4, _5, _6, _7, _8, _9)

  implicit def func1Rep[A1, B](implicit a1: TypeRep[A1], b: TypeRep[B]): TypeRep[Function1[A1, B]] =
    Function1Rep(a1, b)
  implicit def func2Rep[A1, A2, B](implicit a1: TypeRep[A1], a2: TypeRep[A2], b: TypeRep[B]): TypeRep[Function2[A1, A2, B]] =
    Function2Rep(a1, a2, b)
  implicit def func3Rep[A1, A2, A3, B](implicit a1: TypeRep[A1], a2: TypeRep[A2], a3: TypeRep[A3], b: TypeRep[B]): TypeRep[Function3[A1, A2, A3, B]] =
    Function3Rep(a1, a2, a3, b)
  implicit def func4Rep[A1, A2, A3, A4, B](implicit a1: TypeRep[A1], a2: TypeRep[A2], a3: TypeRep[A3], a4: TypeRep[A4], b: TypeRep[B]): TypeRep[Function4[A1, A2, A3, A4, B]] =
    Function4Rep(a1, a2, a3, a4, b)
  implicit def func5Rep[A1, A2, A3, A4, A5, B](implicit a1: TypeRep[A1], a2: TypeRep[A2], a3: TypeRep[A3], a4: TypeRep[A4], a5: TypeRep[A5], b: TypeRep[B]): TypeRep[Function5[A1, A2, A3, A4, A5, B]] =
    Function5Rep(a1, a2, a3, a4, a5, b)
  implicit def func6Rep[A1, A2, A3, A4, A5, A6, B](implicit a1: TypeRep[A1], a2: TypeRep[A2], a3: TypeRep[A3], a4: TypeRep[A4], a5: TypeRep[A5], a6: TypeRep[A6], b: TypeRep[B]): TypeRep[Function6[A1, A2, A3, A4, A5, A6, B]] =
    Function6Rep(a1, a2, a3, a4, a5, a6, b)
  implicit def func7Rep[A1, A2, A3, A4, A5, A6, A7, B](implicit a1: TypeRep[A1], a2: TypeRep[A2], a3: TypeRep[A3], a4: TypeRep[A4], a5: TypeRep[A5], a6: TypeRep[A6], a7: TypeRep[A7], b: TypeRep[B]): TypeRep[Function7[A1, A2, A3, A4, A5, A6, A7, B]] =
    Function7Rep(a1, a2, a3, a4, a5, a6, a7, b)
  implicit def func8Rep[A1, A2, A3, A4, A5, A6, A7, A8, B](implicit a1: TypeRep[A1], a2: TypeRep[A2], a3: TypeRep[A3], a4: TypeRep[A4], a5: TypeRep[A5], a6: TypeRep[A6], a7: TypeRep[A7], a8: TypeRep[A8], b: TypeRep[B]): TypeRep[Function8[A1, A2, A3, A4, A5, A6, A7, A8, B]] =
    Function8Rep(a1, a2, a3, a4, a5, a6, a7, a8, b)
  implicit def func9Rep[A1, A2, A3, A4, A5, A6, A7, A8, A9, B](implicit a1: TypeRep[A1], a2: TypeRep[A2], a3: TypeRep[A3], a4: TypeRep[A4], a5: TypeRep[A5], a6: TypeRep[A6], a7: TypeRep[A7], a8: TypeRep[A8], a9: TypeRep[A9], b: TypeRep[B]): TypeRep[Function9[A1, A2, A3, A4, A5, A6, A7, A8, A9, B]] =
    Function9Rep(a1, a2, a3, a4, a5, a6, a7, a8, a9, b)
/*
  implicit def objectRep[A <: AnyRef](obj: A)(implicit rep: TypeRep[A]): TypeRep[AnyClass] =
    ObjectRep(obj.getClass)
*/

  case object BooleanRep extends TypeRep[Boolean] {
    override def toString = "Boolean"
  }
  case object ByteRep extends TypeRep[Byte] {
    override def toString = "Byte"
  }
  case object CharRep extends TypeRep[Char] {
    override def toString = "Char"
  }
  case object ShortRep extends TypeRep[Short] {
    override def toString = "Short"
  }
  case object IntRep extends TypeRep[Int] {
    override def toString = "Int"
  }
  case object LongRep extends TypeRep[Long] {
    override def toString = "Long"
  }
  case object FloatRep extends TypeRep[Float] {
    override def toString = "Float"
  }
  case object DoubleRep extends TypeRep[Double] {
    override def toString = "Double"
  }

  case object UnitRep extends TypeRep[Unit] {
    override def toString = "Unit"
  }
//  case object ClassRep extends TypeRep[AnyClass] {
//    override def toString = "Class"
//  }
  case object StringRep extends TypeRep[String] {
    override def toString = "String"
  }
  case object NoneRep extends TypeRep[None.type] {
    override def toString = "None"
  }
  case object NilRep extends TypeRep[Nil.type] {
    override def toString = "Nil"
  }
  case object AnyRep extends TypeRep[Any] {
    override def toString = "Any"
  }
  case object NothingRep extends TypeRep[Nothing] {
    override def toString = "Nothing"
  }

  @serializable
  case class ClassRep[A](elemRep: TypeRep[A]) extends TypeRep[Class[A]] {
    override def toString = "Class[" + elemRep + "]"
  }
  @serializable
  case class SomeRep[A](elemRep: TypeRep[A]) extends TypeRep[Some[A]] {
    override def toString = "Some[" + elemRep + "]"
  }
  @serializable
  case class NoneRep[A](elemRep: TypeRep[A]) extends TypeRep[Option[A]] {
    override def toString = "None[" + elemRep + "]"
  }

  @serializable
  case class ListRep[A](elemRep: TypeRep[A]) extends TypeRep[List[A]] {
    override def toString = "List[" + elemRep + "]"
  }

  @serializable
  case class ArrayRep[A](elemRep: TypeRep[A]) extends TypeRep[Array[A]] {
    override def toString = "Array[" + elemRep + "]"
  }

  @serializable
  case class Tuple2Rep[A1, A2](_1: TypeRep[A1], _2: TypeRep[A2]) extends TypeRep[(A1, A2)] {
    override def toString = "Tuple2[" + _1 + ", " + _2 + "]"
  }
  @serializable
  case class Tuple3Rep[A1, A2, A3](_1: TypeRep[A1], _2: TypeRep[A2], _3: TypeRep[A3]) extends TypeRep[Tuple3[A1, A2, A3]] {
    override def toString = "Tuple3[" + _1 + ", " + _2 + ", " + _3 + "]"
  }
  @serializable
  case class Tuple4Rep[A1, A2, A3, A4](_1: TypeRep[A1], _2: TypeRep[A2], _3: TypeRep[A3], _4: TypeRep[A4]) extends TypeRep[Tuple4[A1, A2, A3, A4]] {
    override def toString = "Tuple4[" + _1 + ", " + _2 + ", " + _3 + ", " + _4 + "]"
  }
  @serializable
  case class Tuple5Rep[A1, A2, A3, A4, A5](_1: TypeRep[A1], _2: TypeRep[A2], _3: TypeRep[A3], _4: TypeRep[A4], _5: TypeRep[A5]) extends TypeRep[Tuple5[A1, A2, A3, A4, A5]] {
    override def toString = "Tuple5[" + _1 + ", " + _2 + ", " + _3 + ", " + _4 + ", " + _5 + "]"
  }
  @serializable
  case class Tuple6Rep[A1, A2, A3, A4, A5, A6](val _1: TypeRep[A1], val _2: TypeRep[A2], val _3: TypeRep[A3], val _4: TypeRep[A4], val _5: TypeRep[A5], val _6: TypeRep[A6]) extends TypeRep[Tuple6[A1, A2, A3, A4, A5, A6]] {
    override def toString = "Tuple6[" + _1 + ", " + _2 + ", " + _3 + ", " + _4 + ", " + _5 + ", " + _6 + "]"
  }
  @serializable
  case class Tuple7Rep[A1, A2, A3, A4, A5, A6, A7](val _1: TypeRep[A1], val _2: TypeRep[A2], val _3: TypeRep[A3], val _4: TypeRep[A4], val _5: TypeRep[A5], val _6: TypeRep[A6], val _7: TypeRep[A7]) extends TypeRep[Tuple7[A1, A2, A3, A4, A5, A6, A7]] {
    override def toString = "Tuple7[" + _1 + ", " + _2 + ", " + _3 + ", " + _4 + ", " + _5 + ", " + _6 + ", " + _7 + "]"
  }
  @serializable
  case class Tuple8Rep[A1, A2, A3, A4, A5, A6, A7, A8](val _1: TypeRep[A1], val _2: TypeRep[A2], val _3: TypeRep[A3], val _4: TypeRep[A4], val _5: TypeRep[A5], val _6: TypeRep[A6], val _7: TypeRep[A7], val _8: TypeRep[A8]) extends TypeRep[Tuple8[A1, A2, A3, A4, A5, A6, A7, A8]] {
    override def toString = "Tuple8[" + _1 + ", " + _2 + ", " + _3 + ", " + _4 + ", " + _5 + ", " + _6 + ", " + _7 + ", " + _8 + "]"
  }
  @serializable
  case class Tuple9Rep[A1, A2, A3, A4, A5, A6, A7, A8, A9](val _1: TypeRep[A1], val _2: TypeRep[A2], val _3: TypeRep[A3], val _4: TypeRep[A4], val _5: TypeRep[A5], val _6: TypeRep[A6], val _7: TypeRep[A7], val _8: TypeRep[A8], val _9: TypeRep[A9]) extends TypeRep[Tuple9[A1, A2, A3, A4, A5, A6, A7, A8, A9]] {
    override def toString = "Tuple9[" + _1 + ", " + _2 + ", " + _3 + ", " + _4 + ", " + _5 + ", " + _6 + ", " + _7 + ", " + _8 + ", " + _9 + "]"
  }

  @serializable
  case class Function1Rep[A1, B](a1: TypeRep[A1], b: TypeRep[B]) extends TypeRep[Function1[A1, B]] {
    override def toString = "Function1[" + a1 + ", " + b + "]"
  }
  @serializable
  case class Function2Rep[A1, A2, B](a1: TypeRep[A1], a2: TypeRep[A2], b: TypeRep[B]) extends TypeRep[Function2[A1, A2, B]] {
    override def toString = "Function2[" + a1 + ", " + a2 + ", " + b + "]"
  }
  @serializable
  case class Function3Rep[A1, A2, A3, B](a1: TypeRep[A1], a2: TypeRep[A2], a3: TypeRep[A3], b: TypeRep[B]) extends TypeRep[Function3[A1, A2, A3, B]] {
    override def toString = "Function3[" + a1 + ", " + a2 + ", " + a3 + ", " + b + "]"
  }
  @serializable
  case class Function4Rep[A1, A2, A3, A4, B](a1: TypeRep[A1], a2: TypeRep[A2], a3: TypeRep[A3], a4: TypeRep[A4], b: TypeRep[B]) extends TypeRep[Function4[A1, A2, A3, A4, B]] {
    override def toString = "Function4[" + a1 + ", " + a2 + ", " + a3 + ", " + a4 + ", " + b + "]"
  }
  @serializable
  case class Function5Rep[A1, A2, A3, A4, A5, B](a1: TypeRep[A1], a2: TypeRep[A2], a3: TypeRep[A3], a4: TypeRep[A4], a5: TypeRep[A5], b: TypeRep[B]) extends TypeRep[Function5[A1, A2, A3, A4, A5, B]] {
    override def toString = "Function5[" + a1 + ", " + a2 + ", " + a3 + ", " + a4 + ", " + a5 + ", " + b + "]"
  }
  @serializable
  case class Function6Rep[A1, A2, A3, A4, A5, A6, B](a1: TypeRep[A1], a2: TypeRep[A2], a3: TypeRep[A3], a4: TypeRep[A4], a5: TypeRep[A5], a6: TypeRep[A6], b: TypeRep[B]) extends TypeRep[Function6[A1, A2, A3, A4, A5, A6, B]] {
    override def toString = "Function6[" + a1 + ", " + a2 + ", " + a3 + ", " + a4 + ", " + a5 + ", " + a6 + ", " + b + "]"
  }
  @serializable
  case class Function7Rep[A1, A2, A3, A4, A5, A6, A7, B](a1: TypeRep[A1], a2: TypeRep[A2], a3: TypeRep[A3], a4: TypeRep[A4], a5: TypeRep[A5], a6: TypeRep[A6], a7: TypeRep[A7], b: TypeRep[B]) extends TypeRep[Function7[A1, A2, A3, A4, A5, A6, A7, B]] {
    override def toString = "Function7[" + a1 + ", " + a2 + ", " + a3 + ", " + a4 + ", " + a5 + ", " + a6 + ", " + a7 + ", " + b + "]"
  }
  @serializable
  case class Function8Rep[A1, A2, A3, A4, A5, A6, A7, A8, B](a1: TypeRep[A1], a2: TypeRep[A2], a3: TypeRep[A3], a4: TypeRep[A4], a5: TypeRep[A5], a6: TypeRep[A6], a7: TypeRep[A7], a8: TypeRep[A8], b: TypeRep[B]) extends TypeRep[Function8[A1, A2, A3, A4, A5, A6, A7, A8, B]] {
    override def toString = "Function8[" + a1 + ", " + a2 + ", " + a3 + ", " + a4 + ", " + a5 + ", " + a6 + ", " + a7 + ", " + a8 + b + "]"
  }
  @serializable
  case class Function9Rep[A1, A2, A3, A4, A5, A6, A7, A8, A9, B](a1: TypeRep[A1], a2: TypeRep[A2], a3: TypeRep[A3], a4: TypeRep[A4], a5: TypeRep[A5], a6: TypeRep[A6], a7: TypeRep[A7], a8: TypeRep[A8], a9: TypeRep[A9], b: TypeRep[B]) extends TypeRep[Function9[A1, A2, A3, A4, A5, A6, A7, A8, A9, B]] {
    override def toString = "Function9[" + a1 + ", " + a2 + ", " + a3 + ", " + a4 + ", " + a5 + ", " + a6 + ", " + a7 + ", " + a8 + ", " + b + "]"
  }
/*
  @serializable
  case class ObjectRep[A](c: Class) extends TypeRep[A] {
    override def toString = c.getName
  }
*/
}

