package scala.tools.nsc.util

import scala.tools.nsc.io.JSON._
import annotation.unchecked

abstract class Pickler[T] {

  import Pickler._

  def pickle(x: T): Pickled
  def unpickle(it: PickleIterator): Result[T]

  def ~ [U] (that: => Pickler[U]): Pickler[T ~ U] = seq(this, that)

  def labelled(label: String): Pickler[T] = Pickler.labelled(label, this)
  def wrapped [U] (in: T => U)(out: U => T): Pickler[U] = wrap(this)(in)(out)
  def cond(p: Any => Boolean): CondPickler[T] = conditional(this, p)
  def asClass[U <: T](c: Class[U]): CondPickler[T] = this labelled c.getName cond (c isInstance _)
}

object Pickler {

  var debugMode = false

  type Pickled = JValue
  type PickleIterator = JIterator[Pickled]

  abstract class Result[+T] {
    def map[U](f: T => U): Result[U] = this match {
      case Success(x) => Success(f(x))
      case f: Failure => f
    }
    def flatMap[U](f: T => Result[U]): Result[U] = this match {
      case Success(x) => f(x)
      case f: Failure => f
    }
    def orElse[U >: T](alt: => Result[U]): Result[U] = this match {
      case Success(x) => this
      case f: Failure => alt
    }
  }

  case class Success[T](result: T) extends Result[T]

  class Failure(msg: => String, it: PickleIterator) extends Result[Nothing] {
    override def toString = it.reader match {
      case Some(rdr) => "Failure at "+rdr.tokenPos+": "+msg
      case None => "Failure: "+msg
    }
  }

  def errorExpected(msg: String, it: PickleIterator) =
    new Failure("expected: "+msg+"\n" +
                "found   : "+(if (it.hasNext) it.head.toString else "<end of input>"),
                it)

  def valueIterator(x: JValue): PickleIterator = {
    val xArray = x match {
      case arr: JArray => arr
      case _ => new JArray(x)
    }
    xArray.inputIterator
  }

  def unpickleLabelled[T](it: PickleIterator, label: String)
                         (unpickleBinding: (PickleIterator, String) => Result[T]): Result[T] = {
    def err(it: PickleIterator) = errorExpected("{"+label+": ?}", it)
    if (it.hasNext)
      it.head match {
        case jo: JObject =>
          val it2 = jo.inputIterator
          if (it2.hasNext)
            it2.head match {
              case name J_: rhs if (label == "?" || label == name) =>
                it2.next()
                val result = unpickleBinding(valueIterator(rhs), name)
                it.next()
                result
              case _ =>
                err(it)
            }
          else err(it)
        case _ =>
          err(it)
      }
    else
      err(it)
  }

  def labelled[T](label: String, p: Pickler[T]): Pickler[T] = new Pickler[T] {
    def pickle(x: T): Pickled =
      JObject(label J_: p.pickle(x))
    def unpickle(it: PickleIterator): Result[T] =
      unpickleLabelled(it, label) { (rit, name) => p unpickle rit }
  }

  def wrap[S, T](p: Pickler[S])(in: S => T)(out: T => S) = new Pickler[T] {
    def pickle(x: T) = p.pickle(out(x))
    def unpickle(it: PickleIterator) = p.unpickle(it) map in
  }

  def conditional[T](p: Pickler[T], cond: Any => Boolean) = new CondPickler[T](cond) {
    def pickle(x: T) = p.pickle(x)
    def unpickle(it: PickleIterator) = p.unpickle(it)
  }

  def seq[T, U](p: Pickler[T], q: => Pickler[U]) = new Pickler[T ~ U] {
    private def toSeq(x: JValue): Seq[JValue] = x match {
      case JArray(elems @ _*) => elems
      case _ => Vector(x)
    }
    lazy val qq = q
    def pickle(x: T ~ U) =
      JArray(toSeq(p pickle x.fst) ++ toSeq(qq pickle x.snd): _*)
    def unpickle(it: PickleIterator) =
      for (x <- p.unpickle(it); y <- qq.unpickle(it)) yield x ~ y
  }

  def rep[T](implicit p: Pickler[T]): Pickler[Seq[T]] = new Pickler[Seq[T]] {
    def pickle(xs: Seq[T]): Pickled = JArray(xs map p.pickle: _*)
    def unpickle(it: PickleIterator): Result[Seq[T]] =
      if (it.hasNext)
        for {
          first <- p.unpickle(it)
          rest <- rep(p).unpickle(it)
        } yield first +: rest
      else Success(Seq())
  }

  def either[T, U <: T, V <: T](p: CondPickler[U], q: CondPickler[V]) =
    new CondPickler[T](x => p.canPickle(x) || q.canPickle(x)) {
      override def tryPickle(x: Any) = p.tryPickle(x) orElse q.tryPickle(x)
      def pickle(x: T): Pickled = tryPickle(x) getOrElse {
        throw new Error("no pickler found for a "+x.asInstanceOf[AnyRef].getClass.getName)
      }
      def unpickle(it: PickleIterator) = p.unpickle(it) orElse q.unpickle(it)
    }

  def obj[T <: AnyRef](x: T): CondPickler[T] =
    unit.wrapped { _ => x } { x => () } labelled x.getClass.getName cond (x eq _.asInstanceOf[AnyRef])

  def pkl[T: Pickler] = implicitly[Pickler[T]]

  def anyJavaInstance[T <: AnyRef]: Pickler[T] = new Pickler[T] {
    def pickle(x: T) = JObject(x.getClass.getName J_: JArray())
    def unpickle(it: PickleIterator): Result[T] =
      unpickleLabelled(it, "?") { (rit, name) => Success(Class.forName(name).newInstance().asInstanceOf[T]) }
  }

  def nullable[T >: Null](p: Pickler[T]): Pickler[T] = new Pickler[T] {
    def pickle(x: T) = if (x == null) JNull else p.pickle(x)
    def unpickle(it: PickleIterator): Result[T] =
      if (it.hasNext && it.head == JNull) Success(null)
      else p.unpickle(it)
  }

  case class ~[S, T](fst: S, snd: T)

  class TildeDecorator[S](x: S) {
    def ~ [T](y: T): S ~ T = new ~ (x, y)
  }

  implicit def tildeDecorator[S](x: S): TildeDecorator[S] = new TildeDecorator(x)

  implicit def fromTilde[T1, T2, R](f: (T1, T2) => R): T1 ~ T2 => R = { case x1 ~ x2 => f(x1, x2) }

  implicit def toTilde[T1, T2, S](f: S => Option[(T1, T2)]): S => T1 ~ T2 = { x => (f(x): @unchecked) match { case Some((x1, x2)) => x1 ~ x2 } }

  private def jsonValue[T](it: PickleIterator, kind: String)
                          (matcher: PartialFunction[JValue, T]): Result[T] =
    if (it.hasNext && matcher.isDefinedAt(it.head)) Success(matcher(it.next()))
    else errorExpected(kind+" value", it)

  implicit def bigint: Pickler[BigInt] = new Pickler[BigInt] {
    def pickle(x: BigInt): Pickled = JInteger(x)
    def unpickle(it: PickleIterator): Result[BigInt] = jsonValue(it, "integer") { case JInteger(x) => x }
  }

  implicit def long: Pickler[Long] = new Pickler[Long] {
    def pickle(x: Long): Pickled = JLong(x)
    def unpickle(it: PickleIterator): Result[Long] = jsonValue(it, "long") { case JLong(x) => x }
  }

  implicit def double: Pickler[Double] = new Pickler[Double] {
    def pickle(x: Double): Pickled = JDouble(x)
    def unpickle(it: PickleIterator): Result[Double] = jsonValue(it, "double") { case JDouble(x) => x }
  }

  implicit def boolean: Pickler[Boolean] = new Pickler[Boolean] {
    def pickle(x: Boolean): Pickled = if (x) JTrue else JFalse
    def unpickle(it: PickleIterator): Result[Boolean] = jsonValue(it, "boolean") {
      case JTrue => true
      case JFalse => false
    }
  }

  implicit def unit: Pickler[Unit] = new Pickler[Unit] {
    def pickle(x: Unit): Pickled = JArray()
    def unpickle(it: PickleIterator): Result[Unit] = Success(())
  }

  implicit def string: Pickler[String] = nullable {
    new Pickler[String] {
      def pickle(x: String): Pickled = JString(x)
      def unpickle(it: PickleIterator): Result[String] = jsonValue(it, "string") { case JString(x) => x }
    }
  }

  implicit def byte: Pickler[Byte] = long.wrapped { _.toByte } { _.toLong }
  implicit def short: Pickler[Short] = long.wrapped { _.toShort } { _.toLong }
  implicit def char: Pickler[Char] = long.wrapped { _.toChar } { _.toLong }
  implicit def int: Pickler[Int] = long.wrapped { _.toInt } { _.toLong }
  implicit def float: Pickler[Float] = double.wrapped { _.toFloat } { _.toLong }

  implicit def tuple2[T1, T2](implicit p1: Pickler[T1], p2: Pickler[T2]): Pickler[(T1, T2)] =
    (p1 ~ p2)
      .wrapped { case x1 ~ x2 => (x1, x2) } { case (x1, x2) => x1 ~ x2 }
      .labelled ("tuple2")

  implicit def tuple3[T1, T2, T3](implicit p1: Pickler[T1], p2: Pickler[T2], p3: Pickler[T3]): Pickler[(T1, T2, T3)] =
    (p1 ~ p2 ~ p3)
      .wrapped { case x1 ~ x2 ~ x3 => (x1, x2, x3) } { case (x1, x2, x3) => x1 ~ x2 ~ x3 }
      .labelled ("tuple3")

  implicit def tuple4[T1, T2, T3, T4](implicit p1: Pickler[T1], p2: Pickler[T2], p3: Pickler[T3], p4: Pickler[T4]): Pickler[(T1, T2, T3, T4)] =
    (p1 ~ p2 ~ p3 ~ p4)
      .wrapped { case x1 ~ x2 ~ x3 ~ x4 => (x1, x2, x3, x4) } { case (x1, x2, x3, x4) => x1 ~ x2 ~ x3 ~ x4 }
      .labelled ("tuple3")

  implicit def none = obj(None)

  implicit def some[T](implicit pt: Pickler[T]): CondPickler[Some[T]] =
    pt.wrapped { Some(_) } { _.get }
      .asClass (classOf[Some[T]])

  implicit def option[T](implicit pt: Pickler[T]): Pickler[Option[T]] = none | some[T]

  implicit def list[T](implicit pt: Pickler[T]): Pickler[List[T]] =
    rep[T](pt).wrapped { _.toList } { x => x } .labelled ("scala.List")

  implicit def vector[T](implicit pt: Pickler[T]): Pickler[Vector[T]] =
    rep[T](pt).wrapped { Vector() ++ _  } { x => x } .labelled ("scala.Vector")

  implicit def array[T](implicit ev: ClassManifest[T], pt: Pickler[T]): Pickler[Array[T]] =
    rep[T](pt).wrapped { _.toArray} { _.toSeq } .labelled ("scala.Array")
}

abstract class CondPickler[T](val canPickle: Any => Boolean) extends Pickler[T] {
  import Pickler._
  def tryPickle(x: Any): Option[Pickled] =
    if (canPickle(x)) Some(pickle(x.asInstanceOf[T])) else None
  def | [V >: T, U <: V] (that: CondPickler[U]): CondPickler[V] =
    either[V, T, U](this, that)
}

object Test extends Application {
  import Pickler._
  import scala.tools.nsc.io.JSON._
  import java.io.{StringReader, StringWriter}

  case class Foo(x: Int, y: String)

  implicit lazy val foo: Pickler[Foo] =
    nullable((int ~ string).wrapped { Foo.apply } { toTilde(Foo.unapply) } asClass classOf[Foo])

  case class Rec(x: Int, r: Rec)

  implicit lazy val rec: Pickler[Rec] =
    nullable((int ~ rec).wrapped { Rec.apply } { toTilde(Rec.unapply) } asClass classOf[Rec])

  abstract class L[+T]

  case class Cons[T](head: T, tail: L[T]) extends L[T]
  case object NIL extends L[Nothing]

  implicit def cons[T: Pickler] =
    (pkl[T] ~ pkl[L[T]])
      .wrapped { case x ~ y => Cons(x, y) } { toTilde(Cons.unapply) } asClass classOf[Cons[T]]

  implicit lazy val nil = obj(NIL)

  implicit def l[T: Pickler]: Pickler[L[T]] = cons[T] | nil

  implicit lazy val test = obj(Test)

  implicit def anyThrowableInstance[T <: Throwable]: CondPickler[T] = anyJavaInstance[T] cond { _ => true }

  def testRelaxed[T: Pickler](x: T): T = {
    val sw = new PrettyWriter(new StringWriter())
    val pickler = pkl[T]
    val pickled = pickler pickle x
    pickled.write(sw)
    sw.close()
    val s = sw.toString
    println("pickled: "+s)
    val sr = new JReader(new StringReader(s))
    val r = pickler unpickle sr.iterator
    println("unpickled: "+r)
    val Success(y) = r
    y
  }

  def test[T: Pickler](x: T) {
    assert(testRelaxed(x) == x)
  }

  test(Foo(123, "abc"))
  test(List(1, 2, 3))
  test(Test)
  test(Foo(1, null))
  test(Rec(1, Rec(2, Rec(3, null))))
  test(Cons(1, Cons(2, Cons(3, NIL))))
  testRelaxed(new java.io.IOException)

}

