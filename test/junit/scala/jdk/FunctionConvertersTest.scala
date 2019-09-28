/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.jdk

import java.io.NotSerializableException

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.jdk.FunctionConverters._
import scala.jdk.javaapi.{FunctionConverters => conv}
import scala.language.implicitConversions
import scala.tools.testkit.AssertUtil._

@RunWith(classOf[JUnit4])
class FunctionConvertersTest {
  import java.io.File
  import java.util.function._

  // A series of hacks to enable implicit conversion of `Integer => Integer` to `Int => Int` etc.

  final class Unboxer[B, P]
  object Unboxer extends LPUnboxer {
    implicit val unitUnboxer: Unboxer[scala.runtime.BoxedUnit, Unit] = new Unboxer
    implicit val byteUnboxer: Unboxer[java.lang.Byte, Byte] = new Unboxer
    implicit val shortUnboxer: Unboxer[java.lang.Short, Short] = new Unboxer
    implicit val charUnboxer: Unboxer[java.lang.Character, Char] = new Unboxer
    implicit val intUnboxer: Unboxer[java.lang.Integer, Int] = new Unboxer
    implicit val longUnboxer: Unboxer[java.lang.Long, Long] = new Unboxer
    implicit val floatUnboxer: Unboxer[java.lang.Float, Float] = new Unboxer
    implicit val doubleUnboxer: Unboxer[java.lang.Double, Double] = new Unboxer
    implicit val booleanUnboxer: Unboxer[java.lang.Boolean, Boolean] = new Unboxer

    implicit val unitBoxer: Unboxer[Unit, scala.runtime.BoxedUnit] = new Unboxer
    implicit val byteBoxer: Unboxer[Byte, java.lang.Byte] = new Unboxer
    implicit val shortBoxer: Unboxer[Short, java.lang.Short] = new Unboxer
    implicit val charBoxer: Unboxer[Char, java.lang.Character] = new Unboxer
    implicit val intBoxer: Unboxer[Int, java.lang.Integer] = new Unboxer
    implicit val longBoxer: Unboxer[Long, java.lang.Long] = new Unboxer
    implicit val floatBoxer: Unboxer[Float, java.lang.Float] = new Unboxer
    implicit val doubleBoxer: Unboxer[Double, java.lang.Double] = new Unboxer
    implicit val booleanBoxer: Unboxer[Boolean, java.lang.Boolean] = new Unboxer
  }
  trait LPUnboxer {
    implicit def unUnboxer[T]: Unboxer[T, T] = new Unboxer
  }

  implicit def f0conv[T, U](f: Function0[T])(implicit u: Unboxer[T, U]): Function0[U] = f.asInstanceOf[Function0[U]]
  implicit def f1conv[T1, U1, T2, U2](f: Function1[T1, T2])(implicit u1: Unboxer[T1, U1], u2: Unboxer[T2, U2]): Function1[U1, U2] = f.asInstanceOf[Function1[U1, U2]]
  implicit def f2conv[T1, U1, T2, U2, T3, U3](f: Function2[T1, T2, T3])(implicit u1: Unboxer[T1, U1], u2: Unboxer[T2, U2], u3: Unboxer[T3, U3]): Function2[U1, U2, U3] = f.asInstanceOf[Function2[U1, U2, U3]]

  private val aa = <:<.refl[Any]
  implicit def boxeq[T, U](implicit u: Unboxer[T, U]): T =:= U = aa.asInstanceOf[T =:= U]

  val str = "fish"
  val fyl = new File("salmon")
  val num = 42
  val nmm = 9L
  val nnn = 0.3

  var cache: Any = null
  def save(a: Any) = { cache = a; a }
  def recall = { val ans = cache; cache = null; ans }

  case class Box[A](value: A) {}

  def sameS[A,B,C,D,E,F](f: (A, B) => C, g: (D, E) => F)(implicit ev1: A =:= D, ev2: B =:= E, ev3: C =:= F): Box[(A,B) => Boolean] =
    Box((a: A, b: B) => f(a,b) == g(ev1(a),ev2(b)))

  def sameS[A,B,C,D](f: A => B, g: C => D)(implicit ev1: A =:= C, ev2: B =:= D): Box[A => Boolean] =
    Box((a: A) => f(a) == g(ev1(a)))

  // BiConsumer tests; conceptually widens to BiFunction, narrows to ObjLongConsumer
  @Test
  def test_BiConsumer(): Unit = {
    val bic1 = new BiConsumer[String, File]{ def accept(s: String, f: File): Unit = { save((s,f)) } }
    val bic2 = new BiConsumer[Int, Long]{ def accept(i: Int, l: Long): Unit = { save((i,l)) } }
    val sbic = (s: String, f: File) => { save((s,f)); () }
    val zbic = (i: Int, l: Long) => { save((i,l)); () }
    def jbic[A, B](bic: BiConsumer[A, B])(a: A, b: B) = { bic.accept(a,b); recall == ((a,b)) }
    def fbic[A, B](f: (A,B) => Unit)(a: A, b: B) = { f(a,b); recall == ((a,b)) }
    assert(jbic(conv.asJavaBiConsumer(sbic))(str, fyl))
    assert(jbic(conv.asJavaBiConsumer(zbic))(num, nmm))
    assert(jbic(sbic.asJava)(str, fyl))
    assert(jbic(zbic.asJavaBiConsumer)(num, nmm))
    assert(fbic(conv.asScalaFromBiConsumer(bic1))(str, fyl))
    assert(fbic(conv.asScalaFromBiConsumer(bic2))(num, nmm))
    assert(fbic(bic1.asScala)(str, fyl))
    assert(fbic(bic2.asScala)(num, nmm))
  }


  // BiFunction tests; conceptually narrows to any of the Bi functions or to ObjLongConsumer etc
  @Test
  def test_BiFunction(): Unit = {
    val bif1 = new BiFunction[String, File, (String, File)]{ def apply(s: String, f: File) = (s,f) }
    val bif2 = new BiFunction[Int, Long, Double]{ def apply(i: Int, l: Long) = i.toDouble*l }
    val sbif = (s: String, f: File) => (s,f)
    val zbif = (i: Int, l: Long) => i.toDouble*l
    def sameJ[A,B,C,D,E,F](f: BiFunction[A, B, C], g: BiFunction[D, E, F])(implicit ev1: A =:= D, ev2: B =:= E, ev3: C =:= F) =
      Box((a: A, b: B) => f.apply(a,b) == g.apply(ev1(a), ev2(b)))
    assert(sameJ(bif1, sbif.asJava).value(str,fyl))
    assert(sameJ(bif1, conv.asJavaBiFunction(sbif)).value(str,fyl))
    assert(sameJ(bif2, zbif.asJavaBiFunction).value(num, num))
    assert(sameJ(bif2, conv.asJavaBiFunction(zbif)).value(num,nmm))
    assert(sameS(bif1.asScala, sbif).value(str,fyl))
    assert(sameS(conv.asScalaFromBiFunction(bif1), sbif).value(str,fyl))
    assert(sameS(bif2.asScala, zbif).value(num,nmm))
    assert(sameS(conv.asScalaFromBiFunction(bif2), zbif).value(num,nmm))
  }

  // BinaryOperator tests; actually widens to BiFunction and conceptually narrows to IntBinaryOperator etc.
  @Test
  def test_BinaryOperator(): Unit = {
    val bop1 = new BinaryOperator[String]{ def apply(s: String, t: String) = s + t }
    val bop2 = new BinaryOperator[Int]{ def apply(i: Int, j: Int) = i + j }
    val sbop = (s: String, t: String) => s + t
    val zbop = (i: Int, j: Int) => i + j
    def sameJ[A,B](f: BinaryOperator[A], g: BinaryOperator[B])(implicit ev1: A =:= B) =
      Box((a1: A, a2: A) => f.apply(a1, a2) == g.apply(ev1(a1), ev1(a2)))
    assert(sameJ(bop1, sbop.asJava).value(str,str))
    assert(sameJ(bop1, conv.asJavaBinaryOperator(sbop)).value(str,str))
    assert(sameJ(bop2, zbop.asJavaBinaryOperator).value(num, num))
    assert(sameJ(bop2, conv.asJavaBinaryOperator(zbop)).value(num,num))
    assert(sameS(bop1.asScala, sbop).value(str,str))
    assert(sameS(conv.asScalaFromBinaryOperator(bop1), sbop).value(str,str))
    assert(sameS(bop2.asScala, zbop).value(num,num))
    assert(sameS(conv.asScalaFromBinaryOperator(bop2), zbop).value(num,num))
  }

  // BiPredicate tests; conceptually widens to BiFunction.  Does not narrow (no IntBiPredicate or the like).
  @Test
  def test_BiPredicate(): Unit = {
    val bip1 = new BiPredicate[String, File]{ def test(s: String, f: File) = s == f.getName }
    val bip2 = new BiPredicate[Int, Long]{ def test(i: Int, l: Long) = i == l }
    val sbip = (s: String, f: File) => s == f.getName
    val zbip = (i: Int, l: Long) => i == l
    def sameJ[A,B,C,D](f: BiPredicate[A,B], g: BiPredicate[C,D])(implicit ev1: A =:= C, ev2: B =:= D) =
      Box((a: A, b: B) => f.test(a,b) == g.test(ev1(a), ev2(b)))
    assert(sameJ(bip1, sbip.asJava).value(str,fyl))
    assert(sameJ(bip1, conv.asJavaBiPredicate(sbip)).value(str,fyl))
    assert(sameJ(bip2, zbip.asJava).value(num,nmm))
    assert(sameJ(bip2, conv.asJavaBiPredicate(zbip)).value(num, nmm))
    assert(sameS(bip1.asScala, sbip).value(str,fyl))
    assert(sameS(conv.asScalaFromBiPredicate(bip1), sbip).value(str,fyl))
    assert(sameS(bip2.asScala, zbip).value(num, nmm))
    assert(sameS(conv.asScalaFromBiPredicate(bip2), zbip).value(num,nmm))
  }

  // BooleanSupplier tests; conceptually widens to Supplier and Function.
  @Test
  def test_BooleanSupplier(): Unit = {
    val bsup = new BooleanSupplier{ def getAsBoolean = true }
    val sbup = () => true
    def foo(bs: BooleanSupplier) = bs.getAsBoolean
    def bar(f: () => Boolean) = foo(f.asJava)
    def baz(bs: BooleanSupplier) = bar(bs.asScala)
    assertEquals(foo(bsup), bar(sbup))
    assertEquals(foo(bsup), baz(bsup))
    assertEquals(foo(bsup), bar(conv.asScalaFromBooleanSupplier(bsup)))
    assertEquals(foo(bsup), baz(conv.asJavaBooleanSupplier(sbup)))
  }

  // Consumer tests; conceptually widens to Function and narrows to IntConsumer etc.
  @Test
  def test_Consumer(): Unit = {
    val con1 = new Consumer[String]{ def accept(s: String): Unit = { save(s) } }
    val con2 = new Consumer[Int]{ def accept(i: Int): Unit = { save(i) } }
    val scon = (s: String) => { save(s); () }
    val zcon = (i: Int) => { save(i); () }
    def jcon[A](c: Consumer[A])(a: A) = { c.accept(a); recall == a }
    def fcon[A](f: A => Unit)(a: A) = { f(a); recall == a }
    assert(jcon(scon.asJava)(str))
    assert(jcon(conv.asJavaConsumer(scon))(str))
    assert(jcon(zcon.asJavaConsumer)(num))
    assert(jcon(conv.asJavaConsumer(zcon))(num))
    assert(fcon(con1.asScala)(str))
    assert(fcon(conv.asScalaFromConsumer(con1))(str))
    assert(fcon(con2.asScala)(num))
    assert(fcon(conv.asScalaFromConsumer(con2))(num))
  }

  // DoubleBinaryOperator tests; conceptually widens to BinaryOperator, ToDoubleBiFunction, and BiFunction
  @Test
  def test_DoubleBinaryOperator(): Unit = {
    val dbop = new DoubleBinaryOperator{ def applyAsDouble(a: Double, b: Double) = a*b }
    val sdbo = (a: Double, b: Double) => a*b
    def foo(dbo: DoubleBinaryOperator)(a: Double, b: Double) = dbo.applyAsDouble(a,b)
    def bar(f: (Double, Double) => Double)(a: Double, b: Double) = foo(f.asJava)(a,b)
    def baz(dbo: DoubleBinaryOperator)(a: Double, b: Double) = bar(dbo.asScala)(a,b)
    assertEquals(foo(dbop)(nnn, nnn), bar(sdbo)(nnn, nnn), 1e-9)
    assertEquals(foo(dbop)(nnn, nnn), baz(dbop)(nnn, nnn), 1e-9)
    assertEquals(foo(dbop)(nnn, nnn), bar(conv.asScalaFromDoubleBinaryOperator(dbop))(nnn, nnn), 1e-9)
    assertEquals(foo(dbop)(nnn, nnn), baz(conv.asJavaDoubleBinaryOperator(sdbo))(nnn, nnn), 1e-9)
  }

  // DoubleConsumer tests; conceptually widens to Consumer and Function
  @Test
  def test_DoubleConsumer(): Unit = {
    val dcon = new DoubleConsumer{ def accept(value: Double): Unit = { save(value) } }
    val sdco = (d: Double) => { save(d); () }
    def jf(dc: DoubleConsumer)(d: Double) = { dc.accept(d); recall == d }
    def sf(f: Double => Unit)(d: Double) = { f(d); recall == d }
    assert(jf(sdco.asJava)(nnn))
    assert(jf(conv.asJavaDoubleConsumer(sdco))(nnn))
    assert(sf(dcon.asScala)(nnn))
    assert(sf(conv.asScalaFromDoubleConsumer(dcon))(nnn))
  }

  // DoubleFunction tests; conceptually widens to Function, narrows to DoubleUnaryOperator and DoubleToIntFunction etc.
  @Test
  def test_DoubleFunction(): Unit = {
    val dfn1 = new DoubleFunction[String]{ def apply(value: Double) = f"$value%.3f" }
    val dfn2 = new DoubleFunction[Int]{ def apply(value: Double) = math.ceil(value).toInt }
    val sdfn = (d: Double) => f"$d%.3f"
    val zdfn = (d: Double) => math.ceil(d).toInt
    assertEquals(dfn1(nnn), sdfn(nnn))
    assertEquals(dfn1(nnn), dfn1.asScala(nnn))
    assertEquals(dfn1(nnn), conv.asScalaFromDoubleFunction(dfn1)(nnn))
    assertEquals(dfn1(nnn), sdfn.asJava(nnn))
    assertEquals(dfn1(nnn), conv.asJavaDoubleFunction(sdfn)(nnn))
    assertEquals(dfn2(nnn), zdfn(nnn))
    assertEquals(dfn2(nnn), dfn2.asScala(nnn))
    assertEquals(dfn2(nnn), conv.asScalaFromDoubleFunction(dfn2)(nnn))
    assertEquals(dfn2(nnn), zdfn.asJavaDoubleFunction(nnn))
    assertEquals(dfn2(nnn), conv.asJavaDoubleFunction(zdfn)(nnn))
  }

  // DoublePredicate tests; conceptually widens to DoubleFunction, Predicate, and Function
  @Test
  def test_DoublePredicate(): Unit = {
    val dprd = new DoublePredicate{ def test(value: Double) = value > 0 }
    val sdpr = (d: Double) => d > 0
    def foo(dp: DoublePredicate)(d: Double) = dp.test(d)
    def bar(f: Double => Boolean)(d: Double) = foo(f.asJava)(d)
    def baz(dp: DoublePredicate)(d: Double) = bar(dp.asScala)(d)
    assertEquals(foo(dprd)(nnn), bar(sdpr)(nnn))
    assertEquals(foo(dprd)(nnn), baz(dprd)(nnn))
    assertEquals(foo(dprd)(nnn), bar(conv.asScalaFromDoublePredicate(dprd))(nnn))
    assertEquals(foo(dprd)(nnn), baz(conv.asJavaDoublePredicate(sdpr))(nnn))
  }

  // DoubleSupplier tests; conceptually widens to Supplier and Function
  @Test
  def test_DoubleSupplier(): Unit = {
    val dsup = new DoubleSupplier{ def getAsDouble = 22.0/7 }
    val sdsu = () => 22.0/7
    def foo(ds: DoubleSupplier) = ds.getAsDouble
    def bar(f: () => Double) = foo(f.asJava)
    def baz(ds: DoubleSupplier) = bar(ds.asScala)
    assertEquals(foo(dsup), bar(sdsu), 1e-9)
    assertEquals(foo(dsup), baz(dsup), 1e-9)
    assertEquals(foo(dsup), bar(conv.asScalaFromDoubleSupplier(dsup)), 1e-9)
    assertEquals(foo(dsup), baz(conv.asJavaDoubleSupplier(sdsu)), 1e-9)
  }

  // DoubleToIntFunction tests; conceptually widens to DoubleFunction and Function
  @Test
  def test_DoubleToIntFunction(): Unit = {
    val d2if = new DoubleToIntFunction{ def applyAsInt(value: Double) = math.ceil(value).toInt }
    val sd2i = (d: Double) => math.ceil(d).toInt
    def foo(di: DoubleToIntFunction)(d: Double) = di.applyAsInt(d)
    def bar(f: Double => Int)(d: Double) = foo(f.asJava)(d)
    def baz(di: DoubleToIntFunction)(d: Double) = bar(di.asScala)(d)
    assertEquals(foo(d2if)(nnn), bar(sd2i)(nnn))
    assertEquals(foo(d2if)(nnn), baz(d2if)(nnn))
    assertEquals(foo(d2if)(nnn), bar(conv.asScalaFromDoubleToIntFunction(d2if))(nnn))
    assertEquals(foo(d2if)(nnn), baz(conv.asJavaDoubleToIntFunction(sd2i))(nnn))
  }

  // DoubleToLongFunction tests; conceptually widens to DoubleFunction and Function
  @Test
  def test_DoubleToLongFunction(): Unit = {
    val d2lf = new DoubleToLongFunction{ def applyAsLong(value: Double) = java.lang.Double.doubleToRawLongBits(value) }
    val sd2l = (d: Double) => java.lang.Double.doubleToRawLongBits(d)
    def foo(dl: DoubleToLongFunction)(d: Double) = dl.applyAsLong(d)
    def bar(f: Double => Long)(d: Double) = foo(f.asJava)(d)
    def baz(dl: DoubleToLongFunction)(d: Double) = bar(dl.asScala)(d)
    assertEquals(foo(d2lf)(nnn), bar(sd2l)(nnn))
    assertEquals(foo(d2lf)(nnn), baz(d2lf)(nnn))
    assertEquals(foo(d2lf)(nnn), bar(conv.asScalaFromDoubleToLongFunction(d2lf))(nnn))
    assertEquals(foo(d2lf)(nnn), baz(conv.asJavaDoubleToLongFunction(sd2l))(nnn))
  }

  // DoubleUnaryOperator tests; conceptually widens to DoubleFunction and ToDoubleFunction and Function
  @Test
  def test_DoubleUnaryOperator(): Unit = {
    val duop = new DoubleUnaryOperator{ def applyAsDouble(value: Double) = 1.0 - value }
    val sduo = (d: Double) => 1.0 - d
    def foo(du: DoubleUnaryOperator)(d: Double) = du.applyAsDouble(d)
    def bar(f: Double => Double)(d: Double) = foo(f.asJava)(d)
    def baz(du: DoubleUnaryOperator)(d: Double) = bar(du.asScala)(d)
    assertEquals(foo(duop)(nnn), bar(sduo)(nnn), 1e-9)
    assertEquals(foo(duop)(nnn), baz(duop)(nnn), 1e-9)
    assertEquals(foo(duop)(nnn), bar(conv.asScalaFromDoubleUnaryOperator(duop))(nnn), 1e-9)
    assertEquals(foo(duop)(nnn), baz(conv.asJavaDoubleUnaryOperator(sduo))(nnn), 1e-9)
  }

  // Function tests; conceptually narrows to everything except BiFunction and its conceptual subclasses
  @Test
  def test_Function(): Unit = {
    val fun1 = new Function[String, File]{ def apply(s: String): File = new File(s) }
    val fun2 = new Function[Int, Long]{ def apply(i: Int): Long = ((i.toLong)<<32) | i }
    def sfun = (s: String) => new File(s)
    def zfun = (i: Int) => (i.toLong << 32) | i
    def jf1(f: Function[String, File])(s: String) = f.apply(s)
    def jf2(f: Function[Int, Long])(i: Int) = f.apply(i)
    def sf1(f: String => File)(s: String) = f(s)
    def sf2(f: Int => Long)(i: Int) = f(i)
    val ans = fun1(str)
    assertEquals(ans, sfun(str))
    assertEquals(ans, jf1(fun1)(str))
    assertEquals(ans, sf1(sfun)(str))
    assertEquals(ans, jf1(sfun.asJava)(str))
    assertEquals(ans, sf1(fun1.asScala)(str))
    assertEquals(ans, jf1(conv.asJavaFunction(sfun))(str))
    assertEquals(ans, sf1(conv.asScalaFromFunction(fun1))(str))
    val anz = fun2(num)
    assertEquals(anz, zfun(num))
    assertEquals(anz, jf2(fun2)(num))
    assertEquals(anz, sf2(zfun)(num))
    assertEquals(anz, jf2(zfun.asJavaFunction)(num))
    assertEquals(anz, sf2(fun2.asScala)(num))
    assertEquals(anz, jf2(conv.asJavaFunction(zfun))(num))
    assertEquals(anz, sf2(conv.asScalaFromFunction(fun2))(num))
  }

  // IntBinaryOperator tests; conceptually widens to BinaryOperator, ToIntBiFunction, and BiFunction
  @Test
  def test_IntBinaryOperator(): Unit = {
    val ibop = new IntBinaryOperator{ def applyAsInt(a: Int, b: Int) = a ^ b }
    val sibo = (i: Int, j: Int) => i ^ j
    def foo(ibo: IntBinaryOperator)(a: Int, b: Int) = ibo.applyAsInt(a,b)
    def bar(f: (Int, Int) => Int)(a: Int, b: Int) = foo(f.asJava)(a,b)
    def baz(ibo: IntBinaryOperator)(a: Int, b: Int) = bar(ibo.asScala)(a,b)
    assertEquals(foo(ibop)(num, num), bar(sibo)(num, num))
    assertEquals(foo(ibop)(num, num), baz(ibop)(num, num))
    assertEquals(foo(ibop)(num, num), bar(conv.asScalaFromIntBinaryOperator(ibop))(num, num))
    assertEquals(foo(ibop)(num, num), baz(conv.asJavaIntBinaryOperator(sibo))(num, num))
  }

  // IntConsumer tests; conceptually widens to Consumer and Function
  @Test
  def test_IntConsumer(): Unit = {
    val icon = new IntConsumer{ def accept(i: Int): Unit = { save(i) } }
    val sico = (i: Int) => { save(i); () }
    def jf(ic: IntConsumer)(d: Int) = { ic.accept(d); recall == d }
    def sf(f: Int => Unit)(d: Int) = { f(d); recall == d }
    assert(jf(sico.asJava)(num))
    assert(jf(conv.asJavaIntConsumer(sico))(num))
    assert(sf(icon.asScala)(num))
    assert(sf(conv.asScalaFromIntConsumer(icon))(num))
  }

  // IntFunction tests; conceptually widens to Function
  @Test
  def test_IntFunction(): Unit = {
    val ifn1 = new IntFunction[String]{ def apply(i: Int) = "!"*i }
    val ifn2 = new IntFunction[Long]{ def apply(i: Int) = ((i.toLong) << 32) | i }
    val sifn = (i: Int) => "!"*i
    val zifn = (i: Int) => (i.toLong << 32) | i
    assertEquals(ifn1(num), sifn(num))
    assertEquals(ifn1(num), ifn1.asScala(num))
    assertEquals(ifn1(num), conv.asScalaFromIntFunction(ifn1)(num))
    assertEquals(ifn1(num), sifn.asJava(num))
    assertEquals(ifn1(num), conv.asJavaIntFunction(sifn)(num))
    assertEquals(ifn2(num), zifn(num))
    assertEquals(ifn2(num), ifn2.asScala(num))
    assertEquals(ifn2(num), conv.asScalaFromIntFunction(ifn2)(num))
    assertEquals(ifn2(num), zifn.asJavaIntFunction(num))
    assertEquals(ifn2(num), conv.asJavaIntFunction(zifn)(num))
  }

  // IntPredicate tests; conceptually widens to IntFunction, Predicate, and Function
  @Test
  def test_IntPredicate(): Unit = {
    val iprd = new IntPredicate{ def test(i: Int) = i < 0 }
    val sipr = (i: Int) => i < 0
    def foo(ip: IntPredicate)(d: Int) = ip.test(d)
    def bar(f: Int => Boolean)(d: Int) = foo(f.asJava)(d)
    def baz(ip: IntPredicate)(d: Int) = bar(ip.asScala)(d)
    assertEquals(foo(iprd)(num), bar(sipr)(num))
    assertEquals(foo(iprd)(num), baz(iprd)(num))
    assertEquals(foo(iprd)(num), bar(conv.asScalaFromIntPredicate(iprd))(num))
    assertEquals(foo(iprd)(num), baz(conv.asJavaIntPredicate(sipr))(num))
  }

  // IntSupplier tests; conceptually widens to Supplier and Function
  @Test
  def test_IntSupplier(): Unit = {
    val isup = new IntSupplier{ def getAsInt = 42 }
    val sisu = () => 42
    def foo(ds: IntSupplier) = ds.getAsInt
    def bar(f: () => Int) = foo(f.asJava)
    def baz(ds: IntSupplier) = bar(ds.asScala)
    assertEquals(foo(isup), bar(sisu))
    assertEquals(foo(isup), baz(isup))
    assertEquals(foo(isup), bar(conv.asScalaFromIntSupplier(isup)))
    assertEquals(foo(isup), baz(conv.asJavaIntSupplier(sisu)))
  }

  // IntToDoubleFunction tests; conceptually widens to ToDoubleFunction, IntFunction, and Function
  @Test
  def test_IntToDoubleFunction(): Unit = {
    val i2df = new IntToDoubleFunction{ def applyAsDouble(i: Int) = i + 0.1*i }
    def si2d = (i: Int) => i + 0.1*i
    def foo(id: IntToDoubleFunction)(i: Int) = id.applyAsDouble(i)
    def bar(f: Int => Double)(i: Int) = foo(f.asJava)(i)
    def baz(id: IntToDoubleFunction)(i: Int) = bar(id.asScala)(i)
    assertEquals(foo(i2df)(num), bar(si2d)(num), 1e-9)
    assertEquals(foo(i2df)(num), baz(i2df)(num), 1e-9)
    assertEquals(foo(i2df)(num), bar(conv.asScalaFromIntToDoubleFunction(i2df))(num), 1e-9)
    assertEquals(foo(i2df)(num), baz(conv.asJavaIntToDoubleFunction(si2d))(num), 1e-9)
  }

  // IntToLongFunction tests; conceptually widens to ToLongFunction, IntFunction, and Function
  @Test
  def test_IntToLongFunction(): Unit = {
    val i2lf = new IntToLongFunction { def applyAsLong(i: Int) = (i.toLong << 32) | i }
    val si2l = (i: Int) => (i.toLong << 32) | i
    def foo(il: IntToLongFunction)(d: Int) = il.applyAsLong(d)
    def bar(f: Int => Long)(d: Int) = foo(f.asJava)(d)
    def baz(il: IntToLongFunction)(d: Int) = bar(il.asScala)(d)
    assertEquals(foo(i2lf)(num), bar(si2l)(num))
    assertEquals(foo(i2lf)(num), baz(i2lf)(num))
    assertEquals(foo(i2lf)(num), bar(conv.asScalaFromIntToLongFunction(i2lf))(num))
    assertEquals(foo(i2lf)(num), baz(conv.asJavaIntToLongFunction(si2l))(num))
  }

  // IntUnaryOperator tests; conceptually widens to ToIntFunction, IntFunction, and Function
  @Test
  def test_IntUnaryOperator(): Unit = {
    val iuop = new IntUnaryOperator{ def applyAsInt(i: Int) = ~i }
    val siuo = (i: Int) => ~i
    def foo(iu: IntUnaryOperator)(d: Int) = iu.applyAsInt(d)
    def bar(f: Int => Int)(d: Int) = foo(f.asJava)(d)
    def baz(iu: IntUnaryOperator)(d: Int) = bar(iu.asScala)(d)
    assertEquals(foo(iuop)(num), bar(siuo)(num))
    assertEquals(foo(iuop)(num), baz(iuop)(num))
    assertEquals(foo(iuop)(num), bar(conv.asScalaFromIntUnaryOperator(iuop))(num))
    assertEquals(foo(iuop)(num), baz(conv.asJavaIntUnaryOperator(siuo))(num))
  }

  // LongBinaryOperator tests; conceptually widens to ToLongFunction, LongFunction, and Function
  @Test
  def test_LongBinaryOperator(): Unit = {
    val lbop = new LongBinaryOperator{ def applyAsLong(a: Long, b: Long) = a | b }
    val slbo = (a: Long, b: Long) => a | b
    def foo(lbo: LongBinaryOperator)(a: Long, b: Long) = lbo.applyAsLong(a,b)
    def bar(f: (Long, Long) => Long)(a: Long, b: Long) = foo(f.asJava)(a,b)
    def baz(lbo: LongBinaryOperator)(a: Long, b: Long) = bar(lbo.asScala)(a,b)
    assertEquals(foo(lbop)(nmm, nmm), bar(slbo)(nmm, nmm))
    assertEquals(foo(lbop)(nmm, nmm), baz(lbop)(nmm, nmm))
    assertEquals(foo(lbop)(nmm, nmm), bar(conv.asScalaFromLongBinaryOperator(lbop))(nmm, nmm))
    assertEquals(foo(lbop)(nmm, nmm), baz(conv.asJavaLongBinaryOperator(slbo))(nmm, nmm))
  }

  // LongConsumer tests; conceptually widens to Consumer and Function
  @Test
  def test_LongConsumer(): Unit = {
    val lcon = new LongConsumer{ def accept(l: Long): Unit = { save(l) } }
    val slco = (l: Long) => { save(l); () }
    def jf(lc: LongConsumer)(d: Long) = { lc.accept(d); recall == d }
    def sf(f: Long => Unit)(d: Long) = { f(d); recall == d }
    assert(jf(slco.asJava)(nmm))
    assert(jf(conv.asJavaLongConsumer(slco))(nmm))
    assert(sf(lcon.asScala)(nmm))
    assert(sf(conv.asScalaFromLongConsumer(lcon))(nmm))
  }

  // LongFunction tests; conceptually widens to Function
  @Test
  def test_LongFunction(): Unit = {
    val lfn1 = new LongFunction[String]{ def apply(l: Long) = l.toString }
    val lfn2 = new LongFunction[Int]{ def apply(l: Long) = (l & 0xFFFFFF).toInt }
    val slfn = (l: Long) => l.toString
    val zlfn = (l: Long) => (l & 0xFFFFFF).toInt
    assertEquals(lfn1(nmm), slfn(nmm))
    assertEquals(lfn1(nmm), lfn1.asScala(nmm))
    assertEquals(lfn1(nmm), conv.asScalaFromLongFunction(lfn1)(nmm))
    assertEquals(lfn1(nmm), slfn.asJava(nmm))
    assertEquals(lfn1(nmm), conv.asJavaLongFunction(slfn)(nmm))
    assertEquals(lfn2(nmm), zlfn(nmm))
    assertEquals(lfn2(nmm), lfn2.asScala(nmm))
    assertEquals(lfn2(nmm), conv.asScalaFromLongFunction(lfn2)(nmm))
    assertEquals(lfn2(nmm), zlfn.asJavaLongFunction(nmm))
    assertEquals(lfn2(nmm), conv.asJavaLongFunction(zlfn)(nmm))
  }

  // LongPredicate tests; conceptually widens to LongFunction and Predicate and Function
  @Test
  def test_LongPredicate(): Unit = {
    val lprd = new LongPredicate{ def test(l: Long) = l < 1 }
    val slpr = (l: Long) => l < 1
    def foo(lp: LongPredicate)(d: Long) = lp.test(d)
    def bar(f: Long => Boolean)(d: Long) = foo(f.asJava)(d)
    def baz(lp: LongPredicate)(d: Long) = bar(lp.asScala)(d)
    assertEquals(foo(lprd)(nmm), bar(slpr)(nmm))
    assertEquals(foo(lprd)(nmm), baz(lprd)(nmm))
    assertEquals(foo(lprd)(nmm), bar(conv.asScalaFromLongPredicate(lprd))(nmm))
    assertEquals(foo(lprd)(nmm), baz(conv.asJavaLongPredicate(slpr))(nmm))
  }

  // LongSupplier tests; conceptually widens to ToLongFunction and Supplier and Function
  @Test
  def test_LongSupplier(): Unit = {
    val lsup = new LongSupplier{ def getAsLong = 1000000000000L }
    val slsu = () => 1000000000000L
    def foo(ls: LongSupplier) = ls.getAsLong
    def bar(f: () => Long) = foo(f.asJava)
    def baz(ls: LongSupplier) = bar(ls.asScala)
    assertEquals(foo(lsup), bar(slsu))
    assertEquals(foo(lsup), baz(lsup))
    assertEquals(foo(lsup), bar(conv.asScalaFromLongSupplier(lsup)))
    assertEquals(foo(lsup), baz(conv.asJavaLongSupplier(slsu)))
  }

  // LongToDoubleFunction tests; conceptually widens to ToDoubleFunction, LongFunction, and Function
  @Test
  def test_LongToDoubleFunction(): Unit = {
    val l2df = new LongToDoubleFunction{ def applyAsDouble(l: Long) = l + 1e-4*l }
    def sl2d = (l: Long) => l + 1e-4*l
    def foo(ld: LongToDoubleFunction)(l: Long) = ld.applyAsDouble(l)
    def bar(f: Long => Double)(l: Long) = foo(f.asJava)(l)
    def baz(ld: LongToDoubleFunction)(l: Long) = bar(ld.asScala)(l)
    assertEquals(foo(l2df)(num), bar(sl2d)(num), 1e-9)
    assertEquals(foo(l2df)(num), baz(l2df)(num), 1e-9)
    assertEquals(foo(l2df)(num), bar(conv.asScalaFromLongToDoubleFunction(l2df))(num), 1e-9)
    assertEquals(foo(l2df)(num), baz(conv.asJavaLongToDoubleFunction(sl2d))(num), 1e-9)
  }

  // LongToIntFunction tests; conceptually widens to ToIntFunction, LongFunction, and Function
  @Test
  def test_LongToIntFunction(): Unit = {
    val l2if = new LongToIntFunction{ def applyAsInt(l :Long) = (l & 0xFFFFFF).toInt }
    val sl2i = (l: Long) => (l & 0xFFFFFF).toInt
    def foo(li: LongToIntFunction)(l: Long) = li.applyAsInt(l)
    def bar(f: Long => Int)(l: Long) = foo(f.asJava)(l)
    def baz(li: LongToIntFunction)(l: Long) = bar(li.asScala)(l)
    assertEquals(foo(l2if)(nmm), bar(sl2i)(nmm))
    assertEquals(foo(l2if)(nmm), baz(l2if)(nmm))
    assertEquals(foo(l2if)(nmm), bar(conv.asScalaFromLongToIntFunction(l2if))(nmm))
    assertEquals(foo(l2if)(nmm), baz(conv.asJavaLongToIntFunction(sl2i))(nmm))
  }

  // LongUnaryOperator tests; conceptually widens to LongFunction, ToLongFunction, and Function
  @Test
  def test_LongUnaryOperator(): Unit = {
    val luop = new LongUnaryOperator{ def applyAsLong(l: Long) = -l }
    val sluo = (l: Long) => -l
    def foo(du: LongUnaryOperator)(l: Long) = du.applyAsLong(l)
    def bar(f: Long => Long)(l: Long) = foo(f.asJava)(l)
    def baz(du: LongUnaryOperator)(l: Long) = bar(du.asScala)(l)
    assertEquals(foo(luop)(nmm), bar(sluo)(nmm))
    assertEquals(foo(luop)(nmm), baz(luop)(nmm))
    assertEquals(foo(luop)(nmm), bar(conv.asScalaFromLongUnaryOperator(luop))(nmm))
    assertEquals(foo(luop)(nmm), baz(conv.asJavaLongUnaryOperator(sluo))(nmm))
  }

  // ObjDoubleConsumer tests; conceptually widens to Consumer and BiFunction
  @Test
  def test_ObjDoubleConsumer(): Unit = {
    val odc1 = new ObjDoubleConsumer[String]{ def accept(s: String, d: Double): Unit = { save((s,d)) } }
    val odc2 = new ObjDoubleConsumer[Int]{ def accept(i: Int, d: Double): Unit = { save((i,d)) } }
    val sodc = (s: String, d: Double) => { save((s,d)); () }
    val zodc = (i: Int, d: Double) => { save((i,d)); () }
    def jf1(odc: ObjDoubleConsumer[String])(s: String, d: Double) = { odc.accept(s,d); recall == ((s,d)) }
    def jf2(odc: ObjDoubleConsumer[Int])(i: Int, d: Double) = { odc.accept(i,d); recall == ((i,d)) }
    def sf1(f: (String, Double) => Unit)(s: String, d: Double) = { f(s,d); recall == ((s,d)) }
    def sf2(f: (Int, Double) => Unit)(i: Int, d: Double) = { f(i,d); recall == ((i,d)) }
    assert(jf1(odc1)(str, nnn))
    assert(jf1(sodc.asJava)(str, nnn))
    assert(jf1(conv.asJavaObjDoubleConsumer(sodc))(str, nnn))
    assert(sf1(sodc)(str, nnn))
    assert(sf1(odc1.asScala)(str, nnn))
    assert(sf1(conv.asScalaFromObjDoubleConsumer(odc1))(str, nnn))
    assert(jf2(odc2)(num, nnn))
    assert(jf2(zodc.asJava)(num, nnn))
    assert(jf2(conv.asJavaObjDoubleConsumer(zodc))(num, nnn))
    assert(sf2(zodc)(num, nnn))
    assert(sf2(odc2.asScala)(num, nnn))
    assert(sf2(conv.asScalaFromObjDoubleConsumer(odc2))(num, nnn))
  }

  // ObjIntConsumer tests; conceptually widens to Consumer and BiFunction
  @Test
  def test_ObjIntConsumer(): Unit = {
    val oic1 = new ObjIntConsumer[String]{ def accept(s: String, i: Int): Unit = { save((s,i)) } }
    val oic2 = new ObjIntConsumer[Int]{ def accept(j: Int, i: Int): Unit = { save((j,i)) } }
    val soic = (s: String, i: Int) => { save((s,i)); () }
    val zoic = (j: Int, i: Int) => { save((j,i)); () }
    def jf1(oic: ObjIntConsumer[String])(s: String, i: Int) = { oic.accept(s,i); recall == ((s,i)) }
    def jf2(oic: ObjIntConsumer[Int])(j: Int, i: Int) = { oic.accept(j,i); recall == ((j,i)) }
    def sf1(f: (String, Int) => Unit)(s: String, i: Int) = { f(s,i); recall == ((s,i)) }
    def sf2(f: (Int, Int) => Unit)(j: Int, i: Int) = { f(j,i); recall == ((j,i)) }
    assert(jf1(oic1)(str, num))
    assert(jf1(soic.asJava)(str, num))
    assert(jf1(conv.asJavaObjIntConsumer(soic))(str, num))
    assert(sf1(soic)(str, num))
    assert(sf1(oic1.asScala)(str, num))
    assert(sf1(conv.asScalaFromObjIntConsumer(oic1))(str, num))
    assert(jf2(oic2)(num, num))
    assert(jf2(zoic.asJava)(num, num))
    assert(jf2(conv.asJavaObjIntConsumer(zoic))(num, num))
    assert(sf2(zoic)(num, num))
    assert(sf2(oic2.asScala)(num, num))
    assert(sf2(conv.asScalaFromObjIntConsumer(oic2))(num, num))
  }

  // ObjLongConsumer tests; conceptually widens to Consumer and BiFunction
  @Test
  def test_ObjLongConsumer(): Unit = {
    val olc1 = new ObjLongConsumer[String]{ def accept(s: String, l: Long): Unit = { save((s,l)) } }
    val olc2 = new ObjLongConsumer[Int]{ def accept(i: Int, l: Long): Unit = { save((i,l)) } }
    val solc = (s: String, l: Long) => { save((s,l)); () }
    val zolc = (i: Int, l: Long) => { save((i,l)); () }
    def jf1(olc: ObjLongConsumer[String])(s: String, l: Long) = { olc.accept(s,l); recall == ((s,l)) }
    def jf2(olc: ObjLongConsumer[Int])(i: Int, l: Long) = { olc.accept(i,l); recall == ((i,l)) }
    def sf1(f: (String, Long) => Unit)(s: String, l: Long) = { f(s,l); recall == ((s,l)) }
    def sf2(f: (Int, Long) => Unit)(i: Int, l: Long) = { f(i,l); recall == ((i,l)) }
    assert(jf1(olc1)(str, nmm))
    assert(jf1(solc.asJava)(str, nmm))
    assert(jf1(conv.asJavaObjLongConsumer(solc))(str, nmm))
    assert(sf1(solc)(str, nmm))
    assert(sf1(olc1.asScala)(str, nmm))
    assert(sf1(conv.asScalaFromObjLongConsumer(olc1))(str, nmm))
    assert(jf2(olc2)(num, nmm))
    assert(jf2(zolc.asJava)(num, nmm))
    assert(jf2(conv.asJavaObjLongConsumer(zolc))(num, nmm))
    assert(sf2(zolc)(num, nmm))
    assert(sf2(olc2.asScala)(num, nmm))
    assert(sf2(conv.asScalaFromObjLongConsumer(olc2))(num, nmm))
  }

  // Predicate tests; conceptually widens to Function and narrows to IntPredicate etc.
  @Test
  def test_Predicate(): Unit = {
    val prd1 = new Predicate[String]{ def test(s: String) = s.isEmpty }
    val prd2 = new Predicate[Int]{ def test(i: Int) = i < 0 }
    def sprd = (s: String) => s.isEmpty
    def zprd = (i: Int) => i < 0
    def foos(p: Predicate[String])(s: String) = p.test(s)
    def bars(f: String => Boolean)(s: String) = foos(f.asJava)(s)
    def bazs(p: Predicate[String])(s: String) = bars(p.asScala)(s)
    def fooi(p: Predicate[Int])(i: Int) = p.test(i)
    def bari(f: Int => Boolean)(i: Int) = fooi(conv.asJavaPredicate(f))(i)   // .asScala gives IntPredicate
    def bazi(p: Predicate[Int])(i: Int) = bari(p.asScala)(i)
    assertEquals(foos(prd1)(str), bars(sprd)(str))
    assertEquals(foos(prd1)(str), bazs(prd1)(str))
    assertEquals(foos(prd1)(str), bars(conv.asScalaFromPredicate(prd1))(str))
    assertEquals(foos(prd1)(str), bazs(conv.asJavaPredicate(sprd))(str))
    assertEquals(fooi(prd2)(num), bari(zprd)(num))
    assertEquals(fooi(prd2)(num), bazi(prd2)(num))
    assertEquals(fooi(prd2)(num), bari(conv.asScalaFromPredicate(prd2))(num))
  }

  // Supplier tests; conceptually widens to Function and narrows to IntSupplier etc.
  @Test
  def test_Supplier(): Unit = {
    val sup1 = new Supplier[String]{ def get = "halibut" }
    val sup2 = new Supplier[Int]{ def get = 17 }
    val ssup = () => "halibut"
    val zsup = () => 17
    def foos(s: Supplier[String]) = s.get
    def bars(f: () => String) = foos(f.asJava)
    def bazs(s: Supplier[String]) = bars(s.asScala)
    def fooi(s: Supplier[Int]) = s.get
    def bari(f: () => Int) = fooi(conv.asJavaSupplier(f))  // .asScala gives IntSupplier
    def bazi(s: Supplier[Int]) = bari(s.asScala)
    val ans = foos(sup1)
    assertEquals(ans, bars(ssup))
    assertEquals(ans, bazs(sup1))
    assertEquals(ans, bars(conv.asScalaFromSupplier(sup1)))
    assertEquals(ans, bazs(conv.asJavaSupplier(ssup)))
    val anz = fooi(sup2)
    assertEquals(anz, bari(zsup))
    assertEquals(anz, bazi(sup2))
    assertEquals(anz, bari(conv.asScalaFromSupplier(sup2)))
  }

  // ToDoubleBiFunction tests; conceptually widens to BiFunction and narrows to DoubleBinaryOperator
  @Test
  def test_ToDoubleBiFunction(): Unit = {
    {
      val bfd1 = new ToDoubleBiFunction[String, File]{ def applyAsDouble(s: String, f: File) = s.length.toDouble * f.getName.length }
      val sbfd = (s: String, f: File) => s.length.toDouble * f.getName.length
      def jf1(tdbf: ToDoubleBiFunction[String, File])(s: String, f: File) = tdbf.applyAsDouble(s,f)
      def sf1(f: (String, File) => Double)(s: String, fi: File) = f(s,fi)
      val ans = jf1(bfd1)(str, fyl)
      assertEquals(ans, sf1(sbfd)(str, fyl), 1e-9)
      assertEquals(ans, jf1(sbfd.asJava)(str, fyl), 1e-9)
      assertEquals(ans, sf1(bfd1.asScala)(str, fyl), 1e-9)
      assertEquals(ans, jf1(conv.asJavaToDoubleBiFunction(sbfd))(str, fyl), 1e-9)
      assertEquals(ans, sf1(conv.asScalaFromToDoubleBiFunction(bfd1))(str, fyl), 1e-9)
    }
    {
      val bfd2 = new ToDoubleBiFunction[Double, File]{ def applyAsDouble(a: Double, f: File) = a * f.getName.length }
      val zbfd = (a: Double, f: File) => a * f.getName.length
      def jf2(tdbf: ToDoubleBiFunction[Double, File])(a: Double, f: File) = tdbf.applyAsDouble(a,f)
      def sf2(f: (Double, File) => Double)(a: Double, fi: File) = f(a,fi)
      val ans = jf2(bfd2)(nnn, fyl)
      assertEquals(ans, sf2(zbfd)(nnn, fyl), 1e-9)
      assertEquals(ans, jf2(zbfd.asJava)(nnn, fyl), 1e-9)
      assertEquals(ans, sf2(bfd2.asScala)(nnn, fyl), 1e-9)
      assertEquals(ans, jf2(conv.asJavaToDoubleBiFunction(zbfd))(nnn, fyl), 1e-9)
      assertEquals(ans, sf2(conv.asScalaFromToDoubleBiFunction(bfd2))(nnn, fyl), 1e-9)
    }
  }


  // ToDoubleFunction tests; conceptually widens to Function and narrows to DoubleUnaryOperator, IntToDoubleFunction, etc.
  @Test
  def test_ToDoubleFunction(): Unit = {
    {
      val fnd1 = new ToDoubleFunction[String]{ def applyAsDouble(s: String) = s.length / (s.headOption.getOrElse(0: Char)+1).toDouble }
      val sfnd = (s: String) => s.length / (s.headOption.getOrElse(0: Char)+1).toDouble
      def jf1(tdf: ToDoubleFunction[String])(s: String) = tdf.applyAsDouble(s)
      def sf1(f: String => Double)(s: String) = f(s)
      val ans = jf1(fnd1)(str)
      assertEquals(ans, sf1(sfnd)(str), 1e-9)
      assertEquals(ans, jf1(sfnd.asJava)(str), 1e-9)
      assertEquals(ans, sf1(fnd1.asScala)(str), 1e-9)
      assertEquals(ans, jf1(conv.asJavaToDoubleFunction(sfnd))(str), 1e-9)
      assertEquals(ans, sf1(conv.asScalaFromToDoubleFunction(fnd1))(str), 1e-9)
    }
    {
      val fnd2 = new ToDoubleFunction[Double]{ def applyAsDouble(d: Double) = 1.0 - d }
      val zfnd = (d: Double) => 1.0 - d
      def jf2(tdf: ToDoubleFunction[Double])(x: Double) = tdf.applyAsDouble(x)
      def sf2(f: Double => Double)(x: Double) = f(x)
      val ans = jf2(fnd2)(nnn)
      assertEquals(ans, sf2(zfnd)(nnn), 1e-9)
       assertEquals(ans, jf2(zfnd.asJavaToDoubleFunction)(nnn), 1e-9)
      assertEquals(ans, sf2(conv.asScalaFromToDoubleFunction(fnd2))(nnn), 1e-9)
      assertEquals(ans, jf2(conv.asJavaToDoubleFunction(zfnd))(nnn), 1e-9)
    }
  }

  // ToIntBiFunction tests; conceptually widens to BiFunction and narrows to IntBinaryOperator
  @Test
  def test_ToIntBiFunction(): Unit = {
    {
      val bfi1 = new ToIntBiFunction[String, File]{ def applyAsInt(s: String, f: File) = s.length + f.getName.length }
      val sbfi = (s: String, f: File) => s.length.toInt + f.getName.length
      def jf1(tdbf: ToIntBiFunction[String, File])(s: String, f: File) = tdbf.applyAsInt(s,f)
      def sf1(f: (String, File) => Int)(s: String, fi: File) = f(s,fi)
      val ans = jf1(bfi1)(str, fyl)
      assertEquals(ans, sf1(sbfi)(str, fyl))
      assertEquals(ans, jf1(sbfi.asJava)(str, fyl))
      assertEquals(ans, sf1(bfi1.asScala)(str, fyl))
      assertEquals(ans, jf1(conv.asJavaToIntBiFunction(sbfi))(str, fyl))
      assertEquals(ans, sf1(conv.asScalaFromToIntBiFunction(bfi1))(str, fyl))
    }
    {
      val bfi2 = new ToIntBiFunction[Int, File]{ def applyAsInt(i: Int, f: File) = i * f.getName.length }
      val zbfi = (a: Int, f: File) => a * f.getName.length
      def jf2(tdbf: ToIntBiFunction[Int, File])(a: Int, f: File) = tdbf.applyAsInt(a,f)
      def sf2(f: (Int, File) => Int)(a: Int, fi: File) = f(a,fi)
      val ans = jf2(bfi2)(num, fyl)
      assertEquals(ans, sf2(zbfi)(num, fyl))
      assertEquals(ans, jf2(zbfi.asJava)(num, fyl))
      assertEquals(ans, sf2(bfi2.asScala)(num, fyl))
      assertEquals(ans, jf2(conv.asJavaToIntBiFunction(zbfi))(num, fyl))
      assertEquals(ans, sf2(conv.asScalaFromToIntBiFunction(bfi2))(num, fyl))
    }
  }

  // ToIntFunction tests; conceptually widens to Function and narrows to IntUnaryOperator, etc..
  @Test
  def test_ToIntFunction(): Unit = {
    {
      val fni1 = new ToIntFunction[String]{ def applyAsInt(s: String) = s.length }
      val sfni = (s: String) => s.length
      def jf1(tdf: ToIntFunction[String])(s: String) = tdf.applyAsInt(s)
      def sf1(f: String => Int)(s: String) = f(s)
      val ans = jf1(fni1)(str)
      assertEquals(ans, sf1(sfni)(str))
      assertEquals(ans, jf1(sfni.asJava)(str))
      assertEquals(ans, sf1(fni1.asScala)(str))
      assertEquals(ans, jf1(conv.asJavaToIntFunction(sfni))(str))
      assertEquals(ans, sf1(conv.asScalaFromToIntFunction(fni1))(str))
    }
    {
      val fni2 = new ToIntFunction[Int]{ def applyAsInt(i: Int) = -i }
      val zfni = (x: Int) => -x
      def jf2(tdf: ToIntFunction[Int])(x: Int) = tdf.applyAsInt(x)
      def sf2(f: Int => Int)(x: Int) = f(x)
      val ans = jf2(fni2)(num)
      assertEquals(ans, sf2(zfni)(num))
      assertEquals(ans, jf2(zfni.asJavaToIntFunction)(num))
      assertEquals(ans, sf2(conv.asScalaFromToIntFunction(fni2))(num))
      assertEquals(ans, jf2(conv.asJavaToIntFunction(zfni))(num))
    }
  }

  // ToLongBiFunction tests; conceptually widens to BiFunction and narrows to LongBinaryOperator
  @Test
  def test_ToLongBiFunction(): Unit = {
    {
      val bfl1 = new ToLongBiFunction[String, File]{ def applyAsLong(s: String, f: File) = s.length * f.getName.length }
      val sbfl = (s: String, f: File) => s.length.toLong * f.getName.length
      def jf1(tdbf: ToLongBiFunction[String, File])(s: String, f: File) = tdbf.applyAsLong(s,f)
      def sf1(f: (String, File) => Long)(s: String, fi: File) = f(s,fi)
      val ans = jf1(bfl1)(str, fyl)
      assertEquals(ans, sf1(sbfl)(str, fyl))
      assertEquals(ans, jf1(sbfl.asJava)(str, fyl))
      assertEquals(ans, sf1(bfl1.asScala)(str, fyl))
      assertEquals(ans, jf1(conv.asJavaToLongBiFunction(sbfl))(str, fyl))
      assertEquals(ans, sf1(conv.asScalaFromToLongBiFunction(bfl1))(str, fyl))
    }
    {
      val bfl2 = new ToLongBiFunction[Long, File]{ def applyAsLong(l: Long, f: File) = l ^ f.getName.length }
      val zbfl = (a: Long, f: File) => a ^ f.getName.length
      def jf2(tdbf: ToLongBiFunction[Long, File])(a: Long, f: File) = tdbf.applyAsLong(a,f)
      def sf2(f: (Long, File) => Long)(a: Long, fi: File) = f(a,fi)
      val ans = jf2(bfl2)(nmm, fyl)
      assertEquals(ans, sf2(zbfl)(nmm, fyl))
      assertEquals(ans, jf2(zbfl.asJava)(nmm, fyl))
      assertEquals(ans, sf2(bfl2.asScala)(nmm, fyl))
      assertEquals(ans, jf2(conv.asJavaToLongBiFunction(zbfl))(nmm, fyl))
      assertEquals(ans, sf2(conv.asScalaFromToLongBiFunction(bfl2))(nmm, fyl))
    }
  }

  // ToLongFunction tests; conceptually widens to Function and narrows to LongUnaryOperator, LongToIntFunction etc..
  @Test
  def test_ToLongFunction(): Unit = {
    {
      val fnl1 = new ToLongFunction[String]{ def applyAsLong(s: String) = s.length.toLong << 16 }
      val sfnl = (s: String) => s.length.toLong << 16
      def jf1(tdf: ToLongFunction[String])(s: String) = tdf.applyAsLong(s)
      def sf1(f: String => Long)(s: String) = f(s)
      val ans = jf1(fnl1)(str)
      assertEquals(ans, sf1(sfnl)(str))
      assertEquals(ans, jf1(sfnl.asJava)(str))
      assertEquals(ans, sf1(fnl1.asScala)(str))
      assertEquals(ans, jf1(conv.asJavaToLongFunction(sfnl))(str))
      assertEquals(ans, sf1(conv.asScalaFromToLongFunction(fnl1))(str))
    }
    {
      val fnl2 = new ToLongFunction[Long]{ def applyAsLong(l: Long) = 2 - l }
      val zfnl = (x: Long) => 2 - x
      def jf2(tdf: ToLongFunction[Long])(x: Long) = tdf.applyAsLong(x)
      def sf2(f: Long => Long)(x: Long) = f(x)
      val ans = jf2(fnl2)(num)
      assertEquals(ans, sf2(zfnl)(num))
      assertEquals(ans, jf2(zfnl.asJavaToLongFunction)(num))
      assertEquals(ans, sf2(conv.asScalaFromToLongFunction(fnl2))(num))
      assertEquals(ans, jf2(conv.asJavaToLongFunction(zfnl))(num))
    }
  }

  // UnaryOperator tests; actually widens to Function and conceptually narrows to IntUnaryOperator etc..
  @Test
  def test_UnaryOperator(): Unit = {
    {
      val uop1 = new UnaryOperator[String]{ def apply(s: String) = s.toUpperCase }
      val suop = (s: String) => s.toUpperCase
      def foo(uo: UnaryOperator[String])(s: String) = uo(s)
      def bar(f: String => String)(s: String) = foo(f.asJava)(s)
      def baz(uo: UnaryOperator[String])(s: String) = bar(uo.asScala)(s)
      assertEquals(foo(uop1)(str), bar(suop)(str))
      assertEquals(foo(uop1)(str), baz(uop1)(str))
      assertEquals(foo(uop1)(str), bar(conv.asScalaFromUnaryOperator(uop1))(str))
      assertEquals(foo(uop1)(str), baz(conv.asJavaUnaryOperator(suop))(str))
    }
    {
      val uop2 = new UnaryOperator[Int]{ def apply(i: Int) = -i }
      def zuop = (i: Int) => -i
      def foo(uo: UnaryOperator[Int])(i: Int) = uo(i)
      def bar(f: Int => Int)(i: Int) = foo(conv.asJavaUnaryOperator(f))(i)  // .asScala gives IntUnaryOperator
      def baz(uo: UnaryOperator[Int])(i: Int) = bar(uo.asScala)(i)
      assertEquals(foo(uop2)(num), bar(zuop)(num))
      assertEquals(foo(uop2)(num), baz(uop2)(num))
      assertEquals(foo(uop2)(num), bar(conv.asScalaFromUnaryOperator(uop2))(num))
    }
  }

  @Test
  def noDoubleWrapping(): Unit = {
    val sf = (x: Int, y: Int) => x + y
    val jfa: BiFunction[Int, Int, Int] = (x, y) => x + y
    val jfb: IntBinaryOperator = (x, y) => x + y

    assert(sf eq sf.asJava.asScala)
    assert(sf eq sf.asJavaBinaryOperator.asScala)
    assert(sf eq sf.asJavaBiFunction.asScala)
    assert(jfa eq jfa.asScala.asJavaBiFunction)
    assert(jfa ne (jfa.asScala.asJava: IntBinaryOperator)) // produces a IntBinaryOperator
    assert(jfb eq jfb.asScala.asJava)
    assert(jfb ne jfb.asScala.asJavaBinaryOperator)

    assert(sf eq conv.asScalaFromIntBinaryOperator(conv.asJavaIntBinaryOperator(sf)))
    assert(sf eq conv.asScalaFromBinaryOperator(conv.asJavaBinaryOperator(sf)))
    assert(sf eq conv.asScalaFromBiFunction(conv.asJavaBiFunction(sf)))
    assert(jfa eq conv.asJavaBiFunction(conv.asScalaFromBiFunction(jfa)))
    assert(jfa ne conv.asJavaIntBinaryOperator(conv.asScalaFromBiFunction(jfa): (Int, Int) => Int))
    assert(jfb eq conv.asJavaIntBinaryOperator(conv.asScalaFromIntBinaryOperator(jfb)))
    assert(jfb ne conv.asJavaBinaryOperator(conv.asScalaFromIntBinaryOperator(jfb)))
  }

  private def serializeDeserialize[T <: AnyRef](obj: T): T = {
    import java.io._
    val buffer = new ByteArrayOutputStream
    val out = new ObjectOutputStream(buffer)
    out.writeObject(obj)
    val in = new ObjectInputStream(new ByteArrayInputStream(buffer.toByteArray))
    in.readObject.asInstanceOf[T]
  }

  @Test
  def wrappersSerializable(): Unit = {
    val sf = (x: Int, y: Int) => x + y
    val jfa: BiFunction[Int, Int, Int] = (x, y) => x + y
    val jfb: IntBinaryOperator = (x, y) => x + y
    val jfas = new BiFunction[Int, Int, Int] with Serializable {
      def apply(x: Int, y: Int): Int = x + y
    }
    val jfbs = new IntBinaryOperator with Serializable {
      def applyAsInt(x: Int, y: Int): Int = x + y
    }

    serializeDeserialize(sf)
    assertThrows[NotSerializableException](serializeDeserialize(jfa))
    assertThrows[NotSerializableException](serializeDeserialize(jfb))
    serializeDeserialize(jfas)
    serializeDeserialize(jfbs)
    serializeDeserialize(sf.asJava)
    serializeDeserialize(sf.asJavaBiFunction)
    serializeDeserialize(sf.asJavaBinaryOperator)
    assertThrows[NotSerializableException](serializeDeserialize(jfa.asScala))
    assertThrows[NotSerializableException](serializeDeserialize(jfb.asScala))
    serializeDeserialize(jfas.asScala)
    serializeDeserialize(jfbs.asScala)

  }
}
