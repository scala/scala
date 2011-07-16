/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2011 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*-------------------------------------------------------------------------*/

package org.scalacheck

import scala.collection.mutable.ListBuffer
import util.Buildable
import Prop._
import Arbitrary._

trait Choose[T] {
  def choose(min: T, max: T): Gen[T]
}

object Choose {
  import Gen.{fail, parameterized, value}

  implicit val chooseLong: Choose[Long] = new Choose[Long] {
    def choose(low: Long, high: Long) =
      if(low > high || (high-low < 0)) fail
      else parameterized(prms => value(prms.choose(low,high)))
  }

  implicit val chooseDouble: Choose[Double] = new Choose[Double] {
    def choose(low: Double, high: Double) =
      if (low > high || (high-low > Double.MaxValue)) fail
      else parameterized(prms => value(prms.choose(low,high)))
  }

  implicit val chooseInt: Choose[Int] = new Choose[Int] {
    def choose(low: Int, high: Int) =
      chooseLong.choose(low, high).map(_.toInt)
  }

  implicit val chooseByte: Choose[Byte] = new Choose[Byte] {
    def choose(low: Byte, high: Byte) =
      chooseLong.choose(low, high).map(_.toByte)
  }

  implicit val chooseShort: Choose[Short] = new Choose[Short] {
    def choose(low: Short, high: Short) =
      chooseLong.choose(low, high).map(_.toShort)
  }

  implicit val chooseChar: Choose[Char] = new Choose[Char] {
    def choose(low: Char, high: Char) =
      chooseLong.choose(low, high).map(_.toChar)
  }

  implicit val chooseFloat: Choose[Float] = new Choose[Float] {
    def choose(low: Float, high: Float) =
      chooseDouble.choose(low, high).map(_.toFloat)
  }
}


/** Class that represents a generator. */
sealed trait Gen[+T] {

  import Gen.choose

  var label = "" // TODO: Ugly mutable field

  /** Put a label on the generator to make test reports clearer */
  def label(l: String): Gen[T] = {
    label = l
    this
  }

  /** Put a label on the generator to make test reports clearer */
  def :|(l: String) = label(l)

  /** Put a label on the generator to make test reports clearer */
  def |:(l: String) = label(l)

  /** Put a label on the generator to make test reports clearer */
  def :|(l: Symbol) = label(l.toString.drop(1))

  /** Put a label on the generator to make test reports clearer */
  def |:(l: Symbol) = label(l.toString.drop(1))

  def apply(prms: Gen.Params): Option[T]

  def map[U](f: T => U): Gen[U] = Gen(prms => this(prms).map(f)).label(label)

  def map2[U, V](g: Gen[U])(f: (T, U) => V) =
    combine(g)((t, u) => t.flatMap(t => u.flatMap(u => Some(f(t, u)))))

  def map3[U, V, W](gu: Gen[U], gv: Gen[V])(f: (T, U, V) => W) =
    combine3(gu, gv)((t, u, v) => t.flatMap(t => u.flatMap(u => v.flatMap(v => Some(f(t, u, v))))))

  def map4[U, V, W, X](gu: Gen[U], gv: Gen[V], gw: Gen[W])(f: (T, U, V, W) => X) =
    combine4(gu, gv, gw)((t, u, v, w) => t.flatMap(t => u.flatMap(u => v.flatMap(v => w.flatMap(w => Some(f(t, u, v, w)))))))

  def map5[U, V, W, X, Y](gu: Gen[U], gv: Gen[V], gw: Gen[W], gx: Gen[X])(f: (T, U, V, W, X) => Y) =
    combine5(gu, gv, gw, gx)((t, u, v, w, x) => t.flatMap(t => u.flatMap(u => v.flatMap(v => w.flatMap(w => x.flatMap(x => Some(f(t, u, v, w, x))))))))

  def map6[U, V, W, X, Y, Z](gu: Gen[U], gv: Gen[V], gw: Gen[W], gx: Gen[X], gy: Gen[Y])(f: (T, U, V, W, X, Y) => Z) =
    combine6(gu, gv, gw, gx, gy)((t, u, v, w, x, y) => t.flatMap(t => u.flatMap(u => v.flatMap(v => w.flatMap(w => x.flatMap(x => y.flatMap(y => Some(f(t, u, v, w, x, y)))))))))

  def flatMap[U](f: T => Gen[U]): Gen[U] = Gen(prms => for {
    t <- this(prms)
    u <- f(t)(prms)
  } yield u)

  def filter(p: T => Boolean): Gen[T] = Gen(prms => for {
    t <- this(prms)
    u <- if (p(t)) Some(t) else None
  } yield u).label(label)

  def withFilter(p: T => Boolean) = new GenWithFilter[T](this, p)

  final class GenWithFilter[+A](self: Gen[A], p: A => Boolean) {
    def map[B](f: A => B): Gen[B] = self filter p map f
    def flatMap[B](f: A => Gen[B]): Gen[B] = self filter p flatMap f
    def withFilter(q: A => Boolean): GenWithFilter[A] = new GenWithFilter[A](self, x => p(x) && q(x))
  }

  def suchThat(p: T => Boolean): Gen[T] = filter(p)

  def combine[U,V](g: Gen[U])(f: (Option[T],Option[U]) => Option[V]): Gen[V] =
    Gen(prms => f(this(prms), g(prms)))

  def combine3[U, V, W](gu: Gen[U], gv: Gen[V])
      (f: (Option[T], Option[U], Option[V]) => Option[W]) =
    Gen(prms => f(this(prms), gu(prms), gv(prms)))

  def combine4[U, V, W, X](gu: Gen[U], gv: Gen[V], gw: Gen[W])
      (f: (Option[T], Option[U], Option[V], Option[W]) => Option[X]) =
    Gen(prms => f(this(prms), gu(prms), gv(prms), gw(prms)))

  def combine5[U, V, W, X, Y](gu: Gen[U], gv: Gen[V], gw: Gen[W], gx: Gen[X])
      (f: (Option[T], Option[U], Option[V], Option[W], Option[X]) => Option[Y]) =
    Gen(prms => f(this(prms), gu(prms), gv(prms), gw(prms), gx(prms)))

  def combine6[U, V, W, X, Y, Z](gu: Gen[U], gv: Gen[V], gw: Gen[W], gx: Gen[X], gy: Gen[Y])
      (f: (Option[T], Option[U], Option[V], Option[W], Option[X], Option[Y]) => Option[Z]) =
        Gen(prms => f(this(prms), gu(prms), gv(prms), gw(prms), gx(prms), gy(prms)))

  def ap[U](g: Gen[T => U]) = flatMap(t => g.flatMap(u => Gen(p => Some(u(t)))))

  override def toString =
    if(label.length == 0) "Gen()" else "Gen(\"" + label + "\")"

  /** Returns a new property that holds if and only if both this
   *  and the given generator generates the same result, or both
   *  generators generate no result.
   *  @deprecated Use <code>==</code> instead */
  @deprecated("Use == instead", "1.7")
  def ===[U](g: Gen[U]): Prop = this == g

  /** Returns a new property that holds if and only if both this
   *  and the given generator generates the same result, or both
   *  generators generate no result.  */
  def ==[U](g: Gen[U]) = Prop(prms =>
    (this(prms.genPrms), g(prms.genPrms)) match {
      case (None,None) => proved(prms)
      case (Some(r1),Some(r2)) if r1 == r2 => proved(prms)
      case _ => falsified(prms)
    }
  )

  def !=[U](g: Gen[U]) = forAll(this)(r => forAll(g)(_ != r))

  def !==[U](g: Gen[U]) = Prop(prms =>
    (this(prms.genPrms), g(prms.genPrms)) match {
      case (None,None) => falsified(prms)
      case (Some(r1),Some(r2)) if r1 == r2 => falsified(prms)
      case _ => proved(prms)
    }
  )

  private var freq = 1
  def |[U >: T](g: Gen[U]): Gen[U] = {
    val h = Gen.frequency((freq, this), (1, g))
    h.freq = freq+1
    h
  }

  /** Generates a sample value by using default parameters */
  def sample: Option[T] = apply(Gen.Params())

}


/** Contains combinators for building generators. */
object Gen {

  import Arbitrary._
  import Shrink._

  /** Record that encapsulates all parameters required for data generation */
  case class Params(
    size: Int = 100,
    rng: java.util.Random = util.StdRand
  ) {
    def resize(newSize: Int) = this.copy(size = newSize)

    /** @throws IllegalArgumentException if l is greater than h, or if
     *  the range between l and h doesn't fit in a Long. */
    def choose(l: Long, h: Long): Long = {
      val d = h-l
      if (d < 0) throw new IllegalArgumentException("Invalid range")
      else l + math.abs(rng.nextLong % (d+1))
    }

    /** @throws IllegalArgumentException if l is greater than h, or if
     *  the range between l and h doesn't fit in a Double. */
    def choose(l: Double, h: Double) = {
      val d = h-l
      if (d < 0 || d > Double.MaxValue)
        throw new IllegalArgumentException("Invalid range")
      else if (d == 0) l
      else rng.nextDouble * (h-l) + l
    }
  }

  /* Default generator parameters
   *  @deprecated Use <code>Gen.Params()</code> instead */
  @deprecated("Use Gen.Params() instead", "1.8")
  val defaultParams = Params()

  /* Generator factory method */
  def apply[T](g: Gen.Params => Option[T]) = new Gen[T] {
    def apply(p: Gen.Params) = g(p)
  }

  /* Convenience method for using the <code>frequency</code> method like this:
   * <code>frequency((1, "foo"), (3, "bar"))</code> */
  implicit def freqTuple[T](t: (Int, T)): (Int, Gen[T]) = (t._1, value(t._2))


  //// Various Generator Combinators ////

  /** Sequences generators. If any of the given generators fails, the
   *  resulting generator will also fail. */
  def sequence[C[_],T](gs: Iterable[Gen[T]])(implicit b: Buildable[T,C]): Gen[C[T]] = Gen(prms => {
    val builder = b.builder
    var none = false
    val xs = gs.iterator
    while(xs.hasNext && !none) xs.next.apply(prms) match {
      case None => none = true
      case Some(x) => builder += x
    }
    if(none) None else Some(builder.result())
  })

  /** Wraps a generator lazily. The given parameter is only evalutated once,
   *  and not until the wrapper generator is evaluated. */
  def lzy[T](g: => Gen[T]) = new Gen[T] {
    lazy val h = g
    def apply(prms: Params) = h(prms)
  }

  /** Wraps a generator for later evaluation. The given parameter is
   *  evaluated each time the wrapper generator is evaluated. */
  def wrap[T](g: => Gen[T]) = Gen(p => g(p))

  /** A generator that always generates the given value */
  implicit def value[T](x: T) = Gen(p => Some(x))

  /** A generator that never generates a value */
  def fail[T]: Gen[T] = Gen(p => None)

  /** A generator that generates a random value in the given (inclusive)
   *  range. If the range is invalid, the generator will not generate any value.
   */
  def choose[T](min: T, max: T)(implicit c: Choose[T]): Gen[T] = {
    c.choose(min, max)
  }

  /** Creates a generator that can access its generation parameters */
  def parameterized[T](f: Params => Gen[T]): Gen[T] = Gen(prms => f(prms)(prms))

  /** Creates a generator that can access its generation size */
  def sized[T](f: Int => Gen[T]) = parameterized(prms => f(prms.size))

  /** Creates a resized version of a generator */
  def resize[T](s: Int, g: Gen[T]) = Gen(prms => g(prms.resize(s)))

  /** Chooses one of the given generators with a weighted random distribution */
  def frequency[T](gs: (Int,Gen[T])*): Gen[T] = {
    lazy val tot = (gs.map(_._1) :\ 0) (_+_)

    def pick(n: Int, l: List[(Int,Gen[T])]): Gen[T] = l match {
      case Nil => fail
      case (k,g)::gs => if(n <= k) g else pick(n-k, gs)
    }

    for {
      n <- choose(1,tot)
      x <- pick(n,gs.toList)
    } yield x
  }

  /** Picks a random value from a list */
  def oneOf[T](xs: Seq[T]): Gen[T] = if(xs.isEmpty) fail else for {
    i <- choose(0, xs.size-1)
  } yield xs(i)

  /** Picks a random generator from a list */
  def oneOf[T](g1: Gen[T], g2: Gen[T], gs: Gen[T]*) = for {
    i <- choose(0, gs.length+1)
    x <- if(i == 0) g1 else if(i == 1) g2 else gs(i-2)
  } yield x

  /** Chooses one of the given values, with a weighted random distribution.
   *  @deprecated Use <code>frequency</code> with constant generators
   *  instead. */
  @deprecated("Use 'frequency' with constant generators instead.", "1.6")
  def elementsFreq[T](vs: (Int, T)*): Gen[T] =
    frequency(vs.map { case (w,v) => (w, value(v)) } : _*)

  /** A generator that returns a random element from a list
   *  @deprecated Use <code>oneOf</code> with constant generators instead. */
  @deprecated("Use 'oneOf' with constant generators instead.", "1.6")
  def elements[T](xs: T*): Gen[T] = if(xs.isEmpty) fail else for {
    i <- choose(0,xs.length-1)
  } yield xs(i)


  //// List Generators ////

  /** Generates a container of any type for which there exists an implicit
   *  <code>Buildable</code> instance. The elements in the container will
   *  be generated by the given generator. The size of the generated container
   *  is given by <code>n</code>. */
  def containerOfN[C[_],T](n: Int, g: Gen[T])(implicit b: Buildable[T,C]
  ): Gen[C[T]] = sequence[C,T](new Iterable[Gen[T]] {
    def iterator = new Iterator[Gen[T]] {
      var i = 0
      def hasNext = i < n
      def next = { i += 1; g }
    }
  })

  /** Generates a container of any type for which there exists an implicit
   *  <code>Buildable</code> instance. The elements in the container will
   *  be generated by the given generator. The size of the container is
   *  bounded by the size parameter used when generating values. */
  def containerOf[C[_],T](g: Gen[T])(implicit b: Buildable[T,C]): Gen[C[T]] =
    sized(size => for(n <- choose(0,size); c <- containerOfN[C,T](n,g)) yield c)

  /** Generates a non-empty container of any type for which there exists an
   *  implicit <code>Buildable</code> instance. The elements in the container
   *  will be generated by the given generator. The size of the container is
   *  bounded by the size parameter used when generating values. */
  def containerOf1[C[_],T](g: Gen[T])(implicit b: Buildable[T,C]): Gen[C[T]] =
    sized(size => for(n <- choose(1,size); c <- containerOfN[C,T](n,g)) yield c)

  /** Generates a list of random length. The maximum length depends on the
   *  size parameter. This method is equal to calling
   *  <code>containerOf[List,T](g)</code>. */
  def listOf[T](g: => Gen[T]) = containerOf[List,T](g)

  /** Generates a non-empty list of random length. The maximum length depends
   *  on the size parameter. This method is equal to calling
   *  <code>containerOf1[List,T](g)</code>. */
  def listOf1[T](g: => Gen[T]) = containerOf1[List,T](g)

  /** Generates a list of the given length. This method is equal to calling
   *  <code>containerOfN[List,T](n,g)</code>. */
  def listOfN[T](n: Int, g: Gen[T]) = containerOfN[List,T](n,g)

  /** Generates a list of the given length. This method is equal to calling
   *  <code>containerOfN[List,T](n,g)</code>.
   *  @deprecated Use the method <code>listOfN</code> instead. */
  @deprecated("Use 'listOfN' instead.", "1.6")
  def vectorOf[T](n: Int, g: Gen[T]) = containerOfN[List,T](n,g)

  /** A generator that picks a random number of elements from a list */
  def someOf[T](l: Iterable[T]) = choose(0,l.size) flatMap (pick(_,l))

  /** A generator that picks a random number of elements from a list */
  def someOf[T](g1: Gen[T], g2: Gen[T], gs: Gen[T]*) = for {
    n <- choose(0, gs.length+2)
    x <- pick(n, g1, g2, gs: _*)
  } yield x

  /** A generator that picks a given number of elements from a list, randomly */
  def pick[T](n: Int, l: Iterable[T]): Gen[Seq[T]] =
    if(n > l.size || n < 0) fail
    else Gen(prms => {
      val buf = new ListBuffer[T]
      buf ++= l
      while(buf.length > n) {
        val g = choose(0, buf.length-1)
        buf.remove(g(prms).get)
      }
      Some(buf)
    })

  /** A generator that picks a given number of elements from a list, randomly */
  def pick[T](n: Int, g1: Gen[T], g2: Gen[T], gs: Gen[T]*): Gen[Seq[T]] = for {
    is <- pick(n, 0 until (gs.size+2))
    allGs = gs ++ (g1::g2::Nil)
    xs <- sequence[List,T](is.toList.map(allGs(_)))
  } yield xs


  //// Character Generators ////

  /* Generates a numerical character */
  def numChar: Gen[Char] = choose(48,57) map (_.toChar)

  /* Generates an upper-case alpha character */
  def alphaUpperChar: Gen[Char] = choose(65,90) map (_.toChar)

  /* Generates a lower-case alpha character */
  def alphaLowerChar: Gen[Char] = choose(97,122) map (_.toChar)

  /* Generates an alpha character */
  def alphaChar = frequency((1,alphaUpperChar), (9,alphaLowerChar))

  /* Generates an alphanumerical character */
  def alphaNumChar = frequency((1,numChar), (9,alphaChar))

  //// String Generators ////

  /* Generates a string that starts with a lower-case alpha character,
   * and only contains alphanumerical characters */
  def identifier: Gen[String] = for {
    c <- alphaLowerChar
    cs <- listOf(alphaNumChar)
  } yield (c::cs).mkString

  /* Generates a string of alpha characters */
  def alphaStr: Gen[String] = for(cs <- listOf(Gen.alphaChar)) yield cs.mkString

  /* Generates a string of digits */
  def numStr: Gen[String] = for(cs <- listOf(Gen.numChar)) yield cs.mkString

  //// Number Generators ////

  /* Generates positive integers
   * @deprecated Use <code>posNum[Int]code> instead */
  @deprecated("Use posNum[Int] instead", "1.7")
  def posInt: Gen[Int] = sized(max => choose(1, max))

  /* Generates negative integers
   * @deprecated Use <code>negNum[Int]code> instead */
  @deprecated("Use negNum[Int] instead", "1.7")
  def negInt: Gen[Int] = sized(max => choose(-max, -1))

  /** Generates positive numbers of uniform distribution, with an
   *  upper bound of the generation size parameter. */
  def posNum[T](implicit num: Numeric[T], c: Choose[T]): Gen[T] = {
    import num._
    sized(max => c.choose(one, fromInt(max)))
  }

  /** Generates negative numbers of uniform distribution, with an
   *  lower bound of the negated generation size parameter. */
  def negNum[T](implicit num: Numeric[T], c: Choose[T]): Gen[T] = {
    import num._
    sized(max => c.choose(-fromInt(max), -one))
  }

  /** Generates numbers within the given inclusive range, with
   *  extra weight on zero, +/- unity, both extremities, and any special
   *  numbers provided. The special numbers must lie within the given range,
   *  otherwise they won't be included. */
  def chooseNum[T](minT: T, maxT: T, specials: T*)(
    implicit num: Numeric[T], c: Choose[T]
  ): Gen[T] = {
    import num._
    val basics = List(minT, maxT, zero, one, -one)
    val basicsAndSpecials = for {
      t <- specials ++ basics if t >= minT && t <= maxT
    } yield (1, value(t))
    val allGens = basicsAndSpecials ++ List(
      (basicsAndSpecials.length, c.choose(minT, maxT))
    )
    frequency(allGens: _*)
  }

  /** Takes a function and returns a generator that generates arbitrary
   *  results of that function by feeding it with arbitrarily generated input
   *  parameters. */
  def resultOf[T,R](f: T => R)(implicit a: Arbitrary[T]): Gen[R] =
    arbitrary[T] map f

  /** Takes a function and returns a generator that generates arbitrary
   *  results of that function by feeding it with arbitrarily generated input
   *  parameters. */
  def resultOf[T1,T2,R](f: (T1,T2) => R)(implicit
    a1: Arbitrary[T1], a2: Arbitrary[T2]
  ): Gen[R] = arbitrary[T1] flatMap { t => resultOf(f(t, _:T2)) }

  /** Takes a function and returns a generator that generates arbitrary
   *  results of that function by feeding it with arbitrarily generated input
   *  parameters. */
  def resultOf[T1,T2,T3,R](f: (T1,T2,T3) => R)(implicit
    a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3]
  ): Gen[R] = arbitrary[T1] flatMap { t => resultOf(f(t, _:T2, _:T3)) }

  /** Takes a function and returns a generator that generates arbitrary
   *  results of that function by feeding it with arbitrarily generated input
   *  parameters. */
  def resultOf[T1,T2,T3,T4,R](f: (T1,T2,T3,T4) => R)(implicit
    a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4]
  ): Gen[R] = arbitrary[T1] flatMap {
    t => resultOf(f(t, _:T2, _:T3, _:T4))
  }

  /** Takes a function and returns a generator that generates arbitrary
   *  results of that function by feeding it with arbitrarily generated input
   *  parameters. */
  def resultOf[T1,T2,T3,T4,T5,R](f: (T1,T2,T3,T4,T5) => R)(implicit
    a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4],
    a5: Arbitrary[T5]
  ): Gen[R] = arbitrary[T1] flatMap {
    t => resultOf(f(t, _:T2, _:T3, _:T4, _:T5))
  }

  /** Takes a function and returns a generator that generates arbitrary
   *  results of that function by feeding it with arbitrarily generated input
   *  parameters. */
  def resultOf[T1,T2,T3,T4,T5,T6,R](
    f: (T1,T2,T3,T4,T5,T6) => R)(implicit
    a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3],
    a4: Arbitrary[T4], a5: Arbitrary[T5], a6: Arbitrary[T6]
  ): Gen[R] = arbitrary[T1] flatMap {
    t => resultOf(f(t, _:T2, _:T3, _:T4, _:T5, _:T6))
  }

  /** Takes a function and returns a generator that generates arbitrary
   *  results of that function by feeding it with arbitrarily generated input
   *  parameters. */
  def resultOf[T1,T2,T3,T4,T5,T6,T7,R](
    f: (T1,T2,T3,T4,T5,T6,T7) => R)(implicit
    a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3],
    a4: Arbitrary[T4], a5: Arbitrary[T5], a6: Arbitrary[T6], a7: Arbitrary[T7]
  ): Gen[R] = arbitrary[T1] flatMap {
    t => resultOf(f(t, _:T2, _:T3, _:T4, _:T5, _:T6, _:T7))
  }

  /** Takes a function and returns a generator that generates arbitrary
   *  results of that function by feeding it with arbitrarily generated input
   *  parameters. */
  def resultOf[T1,T2,T3,T4,T5,T6,T7,T8,R](
    f: (T1,T2,T3,T4,T5,T6,T7,T8) => R)(implicit
    a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4],
    a5: Arbitrary[T5], a6: Arbitrary[T6], a7: Arbitrary[T7], a8: Arbitrary[T8]
  ): Gen[R] = arbitrary[T1] flatMap {
    t => resultOf(f(t, _:T2, _:T3, _:T4, _:T5, _:T6, _:T7, _:T8))
  }

  /** Takes a function and returns a generator that generates arbitrary
   *  results of that function by feeding it with arbitrarily generated input
   *  parameters. */
  def resultOf[T1,T2,T3,T4,T5,T6,T7,T8,T9,R](
    f: (T1,T2,T3,T4,T5,T6,T7,T8,T9) => R)(implicit
    a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4],
    a5: Arbitrary[T5], a6: Arbitrary[T6], a7: Arbitrary[T7], a8: Arbitrary[T8],
    a9: Arbitrary[T9]
  ): Gen[R] = arbitrary[T1] flatMap {
    t => resultOf(f(t, _:T2, _:T3, _:T4, _:T5, _:T6, _:T7, _:T8, _:T9))
  }

}
