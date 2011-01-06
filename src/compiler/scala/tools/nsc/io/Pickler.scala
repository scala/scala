package scala.tools.nsc.io

import annotation.unchecked
import Lexer._
import java.io.Writer

abstract class Pickler[T] {

  import Pickler._

  def pickle(wr: Writer, x: T)
  def unpickle(rd: Lexer): Unpickled[T]

  def ~ [U] (that: => Pickler[U]): Pickler[T ~ U] = seqPickler(this, that)

  def labelled(label: String): Pickler[T] = labelledPickler(label, this)
  def wrapped [U] (in: T => U)(out: U => T): Pickler[U] = wrappedPickler(this)(in)(out)
  def orNull(implicit fromNull: Null <:< T): Pickler[T] = nullablePickler(this)
  def cond(p: Any => Boolean): CondPickler[T] = conditionalPickler(this, p)
  def asClass[U <: T](c: Class[U]): CondPickler[T] = this.labelled(c.getName).cond(c isInstance _)
}

object Pickler {

  var picklerDebugMode = false

  abstract class Unpickled[+T] {
    def map[U](f: T => U): Unpickled[U] = this match {
      case UnpickleSuccess(x) => UnpickleSuccess(f(x))
      case f: UnpickleFailure => f
    }
    def flatMap[U](f: T => Unpickled[U]): Unpickled[U] = this match {
      case UnpickleSuccess(x) => f(x)
      case f: UnpickleFailure => f
    }
    def orElse[U >: T](alt: => Unpickled[U]): Unpickled[U] = this match {
      case UnpickleSuccess(x) => this
      case f: UnpickleFailure => alt
    }
    def requireSuccess: UnpickleSuccess[T] = this match {
      case s @ UnpickleSuccess(x) => s
      case f: UnpickleFailure => throw new Error("Unrecoverable unpickle failure:\n"+f)
    }
  }

  case class UnpickleSuccess[+T](result: T) extends Unpickled[T]

  class UnpickleFailure(msg: => String, rd: Lexer) extends Unpickled[Nothing] {
    override def toString = "Failure at "+rd.tokenPos+":\n"+msg
  }

  private def errorExpected(rd: Lexer, msg: => String) =
    new UnpickleFailure("expected: "+msg+"\n" +
                        "found   : "+rd.token,
                        rd)

  private def nextSuccess[T](rd: Lexer, result: T) = {
    rd.nextToken()
    UnpickleSuccess(result)
  }

  def pkl[T: Pickler] = implicitly[Pickler[T]]

  case class ~[S, T](fst: S, snd: T)

  class TildeDecorator[S](x: S) {
    def ~ [T](y: T): S ~ T = new ~ (x, y)
  }

  implicit def tildeDecorator[S](x: S): TildeDecorator[S] = new TildeDecorator(x)

  implicit def fromTilde[T1, T2, R](f: (T1, T2) => R): T1 ~ T2 => R = { case x1 ~ x2 => f(x1, x2) }

  implicit def toTilde[T1, T2, S](f: S => Option[(T1, T2)]): S => T1 ~ T2 = { x => (f(x): @unchecked) match { case Some((x1, x2)) => x1 ~ x2 } }

  def labelledPickler[T](label: String, p: Pickler[T]): Pickler[T] = new Pickler[T] {
    def pickle(wr: Writer, x: T) = {
      wr.write(quoted(label));
      wr.write("(")
      p.pickle(wr, x)
      wr.write(")")
    }
    def unpickle(rd: Lexer): Unpickled[T] =
      rd.token match {
        case StringLit(`label`) =>
          rd.nextToken()
          rd.accept('(')
          val result = p.unpickle(rd).requireSuccess
          rd.accept(')')
          result
        case _ =>
          errorExpected(rd, quoted(label)+"(...)")
      }
  }

  def wrappedPickler[S, T](p: Pickler[S])(in: S => T)(out: T => S) = new Pickler[T] {
    def pickle(wr: Writer, x: T) = p.pickle(wr, out(x))
    def unpickle(rd: Lexer) = p.unpickle(rd) map in
  }

  def conditionalPickler[T](p: Pickler[T], cond: Any => Boolean) = new CondPickler[T](cond) {
    def pickle(wr: Writer, x: T) = p.pickle(wr, x)
    def unpickle(rd: Lexer) = p.unpickle(rd)
  }

  def seqPickler[T, U](p: Pickler[T], q: => Pickler[U]) = new Pickler[T ~ U] {
    lazy val qq = q
    def pickle(wr: Writer, x: T ~ U) = {
      p.pickle(wr, x.fst)
      wr.write(',')
      q.pickle(wr, x.snd)
    }
    def unpickle(rd: Lexer) =
      for (x <- p.unpickle(rd); y <- { rd.accept(','); qq.unpickle(rd).requireSuccess })
      yield x ~ y
  }

  def eitherPickler[T, U <: T, V <: T](p: CondPickler[U], q: => CondPickler[V]) =
    new CondPickler[T](x => p.canPickle(x) || q.canPickle(x)) {
      lazy val qq = q
      override def tryPickle(wr: Writer, x: Any): Boolean =
        p.tryPickle(wr, x) || qq.tryPickle(wr, x)
      def pickle(wr: Writer, x: T) =
        require(tryPickle(wr, x),
                "no pickler found for "+x+" of class "+x.asInstanceOf[AnyRef].getClass.getName)
      def unpickle(rd: Lexer) = p.unpickle(rd) orElse qq.unpickle(rd)
    }

  def singletonPickler[T <: AnyRef](x: T): CondPickler[T] =
    unitPickler
      .wrapped { _ => x } { x => () }
      .labelled (x.getClass.getName)
      .cond (x eq _.asInstanceOf[AnyRef])

  def nullablePickler[T](p: Pickler[T])(implicit fromNull: Null <:< T): Pickler[T] = new Pickler[T] {
    def pickle(wr: Writer, x: T) =
      if (x == null) wr.write("null") else p.pickle(wr, x)
    def unpickle(rd: Lexer): Unpickled[T] =
      if (rd.token == NullLit) nextSuccess(rd, fromNull(null))
      else p.unpickle(rd)
  }

  def javaInstancePickler[T <: AnyRef]: Pickler[T] =
    (stringPickler labelled "$new")
      .wrapped { name => Class.forName(name).newInstance().asInstanceOf[T] } { _.getClass.getName }

  implicit def iterPickler[T: Pickler]: Pickler[Iterator[T]] = new Pickler[Iterator[T]] {
    lazy val p = pkl[T]
    def pickle(wr: Writer, xs: Iterator[T]) {
      var first = true
      for (x <- xs) {
        if (first) first = false else wr.write(',')
        p.pickle(wr, x)
      }
    }
    def unpickle(rd: Lexer): Unpickled[Iterator[T]] = UnpickleSuccess(new Iterator[T] {
      var first = true
      def hasNext = {
        val t = rd.token
        t != EOF && t != RParen && t != RBrace && t != RBracket
      }
      def next(): T = {
        if (first) first = false else rd.accept(',')
        p.unpickle(rd).requireSuccess.result
      }
    })
  }

  private def tokenPickler[T](kind: String)(matcher: PartialFunction[Token, T]) = new Pickler[T] {
    def pickle(wr: Writer, x: T) = wr.write(x.toString)
    def unpickle(rd: Lexer) =
      if (matcher isDefinedAt rd.token) nextSuccess(rd, matcher(rd.token))
      else errorExpected(rd, kind)
  }

  implicit val longPickler: Pickler[Long] =
    tokenPickler("integer literal") { case IntLit(s) => s.toLong }
  implicit val doublePickler: Pickler[Double] =
    tokenPickler("floating point literal") { case FloatLit(s) => s.toDouble }

  implicit val bytePickler: Pickler[Byte] = longPickler.wrapped { _.toByte } { _.toLong }
  implicit val shortPickler: Pickler[Short] = longPickler.wrapped { _.toShort } { _.toLong }
  implicit val intPickler: Pickler[Int] = longPickler.wrapped { _.toInt } { _.toLong }
  implicit val floatPickler: Pickler[Float] = doublePickler.wrapped { _.toFloat } { _.toLong }

  private val truePickler =
    tokenPickler("boolean literal") { case TrueLit => true } cond { _ == true }
  private val falsePickler =
    tokenPickler("boolean literal") { case FalseLit => false } cond { _ == false }

  implicit def booleanPickler: Pickler[Boolean] = truePickler | falsePickler

  implicit val unitPickler: Pickler[Unit] = new Pickler[Unit] {
    def pickle(wr: Writer, x: Unit) {}
    def unpickle(rd: Lexer): Unpickled[Unit] = UnpickleSuccess(())
  }

  implicit val stringPickler: Pickler[String] = new Pickler[String] {
    def pickle(wr: Writer, x: String) = wr.write(if (x == null) "null" else quoted(x))
    def unpickle(rd: Lexer) = rd.token match {
      case StringLit(s) => nextSuccess(rd, s)
      case NullLit => nextSuccess(rd, null)
      case _ => errorExpected(rd, "string literal")
    }
  }

  implicit val charPickler: Pickler[Char] =
    stringPickler
      .wrapped { s => require(s.length == 1, "single character string literal expected, but "+quoted(s)+" found"); s(0) } { _.toString }

  implicit def tuple2Pickler[T1: Pickler, T2: Pickler]: Pickler[(T1, T2)] =
    (pkl[T1] ~ pkl[T2])
      .wrapped { case x1 ~ x2 => (x1, x2) } { case (x1, x2) => x1 ~ x2 }
      .labelled ("tuple2")

  implicit def tuple3Pickler[T1, T2, T3](implicit p1: Pickler[T1], p2: Pickler[T2], p3: Pickler[T3]): Pickler[(T1, T2, T3)] =
    (p1 ~ p2 ~ p3)
      .wrapped { case x1 ~ x2 ~ x3 => (x1, x2, x3) } { case (x1, x2, x3) => x1 ~ x2 ~ x3 }
      .labelled ("tuple3")

  implicit def tuple4Pickler[T1, T2, T3, T4](implicit p1: Pickler[T1], p2: Pickler[T2], p3: Pickler[T3], p4: Pickler[T4]): Pickler[(T1, T2, T3, T4)] =
    (p1 ~ p2 ~ p3 ~ p4)
      .wrapped { case x1 ~ x2 ~ x3 ~ x4 => (x1, x2, x3, x4) } { case (x1, x2, x3, x4) => x1 ~ x2 ~ x3 ~ x4 }
      .labelled ("tuple4")

  implicit val nonePickler = singletonPickler(None)

  implicit def somePickler[T: Pickler]: CondPickler[Some[T]] =
    pkl[T]
      .wrapped { Some(_) } { _.get }
      .asClass (classOf[Some[T]])

  implicit def optionPickler[T: Pickler]: Pickler[Option[T]] = nonePickler | somePickler[T]

  implicit def listPickler[T: Pickler]: Pickler[List[T]] =
    iterPickler[T] .wrapped { _.toList } { _.iterator } .labelled ("scala.List")

  implicit def vectorPickler[T: Pickler]: Pickler[Vector[T]] =
    iterPickler[T] .wrapped { Vector() ++ _ } { _.iterator } .labelled ("scala.Vector")

  implicit def array[T : ClassManifest : Pickler]: Pickler[Array[T]] =
    iterPickler[T] .wrapped { _.toArray} { _.iterator } .labelled ("scala.Array")
}

abstract class CondPickler[T](val canPickle: Any => Boolean) extends Pickler[T] {
  import Pickler._
  def tryPickle(wr: Writer, x: Any): Boolean = {
    val result = canPickle(x)
    if (result) pickle(wr, x.asInstanceOf[T])
    result
  }
  def | [V >: T, U <: V] (that: => CondPickler[U]): CondPickler[V] =
    eitherPickler[V, T, U](this, that)
}

object Test extends Application {
  import Pickler._
  import java.io.{StringReader, StringWriter}

  case class Foo(x: Int, y: String)

  implicit val fooPickler: Pickler[Foo] =
    (pkl[Int] ~ pkl[String])
      .wrapped { Foo.apply } { toTilde(Foo.unapply) }
      .asClass (classOf[Foo])

  case class Rec(x: Int, r: Rec)

  implicit val recPickler: Pickler[Rec] =
    (pkl[Int] ~ pkl[Rec])
      .wrapped { Rec.apply } { toTilde(Rec.unapply) }
      .asClass (classOf[Rec])
      .orNull

  abstract class L[+T]

  case class Cons[T](head: T, tail: L[T]) extends L[T]
  case object NIL extends L[Nothing]

  implicit def consPickler[T: Pickler] =
    (pkl[T] ~ pkl[L[T]])
      .wrapped { case x ~ y => Cons(x, y) } { toTilde(Cons.unapply) }
      .asClass (classOf[Cons[T]])

  implicit lazy val nilPickler = singletonPickler(NIL)

  implicit def lPickler[T: Pickler]: Pickler[L[T]] = consPickler[T] | nilPickler

  implicit lazy val testPickler = singletonPickler(Test)

  implicit def anyThrowableInstance[T <: Throwable]: CondPickler[T] = javaInstancePickler[T] cond { _ => true }

  def testRelaxed[T: Pickler](x: T): T = {
    val sw = new PrettyWriter(new StringWriter())
    val pickler = pkl[T]
    val pickled = pickler.pickle(sw, x)
    sw.close()
    val s = sw.toString
    println("pickled: "+s)
    val sr = new Lexer(new StringReader(s))
    val r = pickler.unpickle(sr)
    println("unpickled: "+r)
    val UnpickleSuccess(y) = r
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

