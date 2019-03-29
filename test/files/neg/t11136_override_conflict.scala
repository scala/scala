
import annotation.unchecked.uncheckedVariance

trait Bldr[-A, +To] { self =>
  def result(): To
  def mapResult[NTo](f: To => NTo): Bldr[A, NTo] = new Bldr[A, NTo] {
    def result(): NTo = f(self.result())
  }
}
trait ItFact[+CC[_]] {
  def from[A](source: It[A]): CC[A]
  def newBuilder[A]: Bldr[A, CC[A]]
}

trait DefaultFromSpecific[+A, +CC[_], +C] {
  protected def fromSpecific(coll: It[A @uncheckedVariance]): CC[A @uncheckedVariance] = iterableFactory.from(coll)
  protected def newSpecificBuilder: Bldr[A @uncheckedVariance, CC[A @uncheckedVariance]] = iterableFactory.newBuilder[A]
  def iterableFactory: ItFact[CC]
}

trait ItOps[+A, +CC[_], +C] {
  def it: It[A]

  protected def newSpecificBuilder: Bldr[A @uncheckedVariance, C]
  protected def fromSpecific(coll: It[A @uncheckedVariance]): C
  def iterableFactory: ItFact[CC]

  def filter: C = fromSpecific(it)
  def strictFilter: C = newSpecificBuilder.result()
  def map[B](f: A => B): CC[B] = iterableFactory.newBuilder.result()
}

trait It[+A] extends ItOps[A, It, It[A]] with DefaultFromSpecific[A, It, It[A]] {
  def it: It[A] = this
  def iterableFactory: ItFact[It] = It
}

object It extends ItFact[It] {
  def from[A](source: It[A]): It[A] = new It[A]{}
  def newBuilder[A]: Bldr[A,It[A]] = new Bldr[A, It[A]] { def result(): It[A] = new It[A]{} }
}

trait SqOps[A, +CC[_], +C] extends ItOps[A, CC, C]

trait Sq[A] extends It[A] with SqOps[A, Sq, Sq[A]] with DefaultFromSpecific[A, Sq, Sq[A]] {
  override def iterableFactory: ItFact[Sq] = Sq

  def flup = 0
}

object Sq extends ItFact[Sq] {
  def from[A](source: It[A]): Sq[A] = new Sq[A]{}
  def newBuilder[A]: Bldr[A, Sq[A]] = new Bldr[A, Sq[A]] { def result(): Sq[A] = new Sq[A]{} }
}

trait Acc[A, +CC[X] <: Sq[X], +C <: Sq[A]] extends Sq[A] with SqOps[A, CC, C] {
  protected def fromSpecificImpl(coll: It[A]): C
  protected def newSpecificBuilderImpl: Bldr[A, C]
  protected def iterableFactoryImpl: ItFact[CC]

  protected override def fromSpecific(coll: It[A]): C = fromSpecificImpl(coll)
  protected override def newSpecificBuilder: Bldr[A, C] = newSpecificBuilderImpl
  override def iterableFactory: ItFact[CC] = iterableFactoryImpl
}
trait AnyAcc[A] extends Acc[A, AnyAcc, AnyAcc[A]] {
  protected override def fromSpecificImpl(coll: It[A]): AnyAcc[A] = iterableFactory.from(coll)
  protected override def newSpecificBuilderImpl: Bldr[A, AnyAcc[A]] = iterableFactory.newBuilder
  override def iterableFactoryImpl: ItFact[AnyAcc] = AnyAcc

  def flap = 1
}
object AnyAcc extends ItFact[AnyAcc] {
  def from[A](source: It[A]): AnyAcc[A] = new AnyAcc[A] {}
  def newBuilder[A]: Bldr[A, AnyAcc[A]] = new Bldr[A, AnyAcc[A]] { def result(): AnyAcc[A] = new AnyAcc[A]{} }
}
trait IntAcc extends Acc[Int, AnyAcc, IntAcc] {
  protected override def fromSpecificImpl(coll: It[Int]): IntAcc = new IntAcc{}
  protected override def newSpecificBuilderImpl: Bldr[Int, IntAcc] = new Bldr[Int, IntAcc] { def result(): IntAcc = new IntAcc{} }
  override def iterableFactoryImpl: ItFact[AnyAcc] = AnyAcc
}

class ArDq[A] extends Sq[A]
  with SqOps[A, ArDq, ArDq[A]]
  with ArDqOps[A, ArDq, ArDq[A]]
  with DefaultFromSpecific[A, ArDq, ArDq[A]] {
  override def iterableFactory: ItFact[ArDq] = ArDq
  def empty: ArDq[A] = new ArDq[A]{}
}

object ArDq extends ItFact[ArDq] {
  override def from[A](source: It[A]): ArDq[A] = new ArDq[A]{}
  override def newBuilder[A]: Bldr[A, ArDq[A]] = new Bldr[A, ArDq[A]] { def result(): ArDq[A] = new ArDq[A]{} }
}

trait ArDqOps[A, +CC[_], +C <: AnyRef] extends SqOps[A, CC, C] {
  def empty: C
}

class Qu[A] extends ArDq[A]
  with SqOps[A, Qu, Qu[A]]
  with ArDqOps[A, Qu, Qu[A]]
  with DefaultFromSpecific[A, Qu, Qu[A]] {
  override def iterableFactory: ItFact[Qu] = Qu
  //  should get a missing override error, but don't. if `ArDq` and `Qu` are both traits, the error shows.
  //  override def empty: Qu[A] = new Qu[A]{}
  def bazza = 2
}

object Qu extends ItFact[Qu] {
  override def from[A](source: It[A]): Qu[A] = new Qu[A]{}
  override def newBuilder[A]: Bldr[A, Qu[A]] = new Bldr[A, Qu[A]] { def result(): Qu[A] = new Qu[A]{} }
}

object Test {
  def main(args: Array[String]): Unit = {
    val sq = new Sq[String] { }
    println(sq.filter.flup)
    val ia = new IntAcc{}
    println(((ia.filter: IntAcc).map(_ => ""): AnyAcc[String]).flap)
    (new Qu[String]{}: ArDqOps[String, Qu, Qu[String]]).empty.bazza
  }
}
