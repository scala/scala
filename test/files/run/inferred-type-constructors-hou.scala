package p {
  trait TCon[+CC[X]] {
    def fPublic: CC[Int]                        = ???
    private[p] def fPackagePrivate: CC[Int]     = ???
    protected[p] def fPackageProtected: CC[Int] = ???
  }
  trait Iterable[+A] extends TCon[Iterable]
  trait Set[A] extends Iterable[A] with TCon[Set]
  trait Seq[+A] extends Iterable[A] with TCon[Seq]

  private[p] abstract class AIterable[+A] extends Iterable[A]
  private[p] abstract class ASeq[+A] extends AIterable[A] with Seq[A]
  private[p] abstract class ASet[A] extends AIterable[A] with Set[A]

  package m {
    private[m] abstract class ASeq[A] extends p.ASeq[A] with Seq[A]
    private[m] abstract class ASet[A] extends p.ASet[A] with Set[A]
    trait Set[A] extends p.Set[A] with TCon[Set]
    trait Seq[A] extends p.Seq[A] with TCon[Seq]
    trait BitSet extends ASet[Int]
    trait IntSeq extends ASeq[Int]
  }

  package i {
    private[i] abstract class ASeq[+A] extends p.ASeq[A] with Seq[A]
    private[i] abstract class ASet[A] extends p.ASet[A] with Set[A]
    trait Set[A] extends p.Set[A] with TCon[Set]
    trait Seq[+A] extends p.Seq[A] with TCon[Seq]
    trait BitSet extends ASet[Int]
    trait IntSeq extends ASeq[Int]
  }
}

object Test {
  import scala.reflect.runtime.universe._
  // Complicated by the absence of usable type constructor type tags.
  def extract[A, CC[X]](xs: CC[A]): CC[A] = xs
  def whatis[T: TypeTag](x: T): Unit = {
    val tpe = typeOf[T]
    val access = tpe.typeSymbol.asInstanceOf[scala.reflect.internal.HasFlags].accessString.replaceAllLiterally("package ", "")
    println(f"$access%15s $tpe")
  }

  trait IntIterable extends p.Iterable[Int]
  trait IntSet extends p.Set[Int]
  trait IntSeq extends p.Seq[Int]

  trait MutableIntSet extends p.m.Set[Int]
  trait MutableIntSeq extends p.m.Seq[Int]

  trait ImmutableIntSet extends p.i.Set[Int]
  trait ImmutableIntSeq extends p.i.Seq[Int]

  def f1: IntIterable = null
  def f2: IntSet = null
  def f3: IntSeq = null

  def g1: MutableIntSet = null
  def g2: MutableIntSeq = null
  def g3: p.m.BitSet = null

  def h1: ImmutableIntSeq = null
  def h2: p.i.BitSet = null
  def h3: p.i.IntSeq = null

  def main(args: Array[String]): Unit = {
    whatis(extract(f1))
    whatis(extract(f2))
    whatis(extract(f3))
    whatis(extract(g1))
    whatis(extract(g2))
    whatis(extract(g3))
    whatis(extract(h1))
    whatis(extract(h2))
    whatis(extract(h3))

    whatis(extract(if (true) f1 else f2))
    whatis(extract(if (true) f1 else f3))
    whatis(extract(if (true) f1 else g1))
    whatis(extract(if (true) f1 else g2))
    whatis(extract(if (true) f1 else g3))
    whatis(extract(if (true) f1 else h1))
    whatis(extract(if (true) f1 else h2))
    whatis(extract(if (true) f1 else h3))
    whatis(extract(if (true) f2 else f3))
    whatis(extract(if (true) f2 else g1))
    whatis(extract(if (true) f2 else g2))
    whatis(extract(if (true) f2 else g3))
    whatis(extract(if (true) f2 else h1))
    whatis(extract(if (true) f2 else h2))
    whatis(extract(if (true) f2 else h3))
    whatis(extract(if (true) f3 else g1))
    whatis(extract(if (true) f3 else g2))
    whatis(extract(if (true) f3 else g3))
    whatis(extract(if (true) f3 else h1))
    whatis(extract(if (true) f3 else h2))
    whatis(extract(if (true) f3 else h3))
    whatis(extract(if (true) g1 else g2))
    whatis(extract(if (true) g1 else g3))
    whatis(extract(if (true) g1 else h1))
    whatis(extract(if (true) g1 else h2))
    whatis(extract(if (true) g1 else h3))
    whatis(extract(if (true) g2 else g3))
    whatis(extract(if (true) g2 else h1))
    whatis(extract(if (true) g2 else h2))
    whatis(extract(if (true) g2 else h3))
    whatis(extract(if (true) g3 else h1))
    whatis(extract(if (true) g3 else h2))
    whatis(extract(if (true) g3 else h3))
    whatis(extract(if (true) h1 else h2))
    whatis(extract(if (true) h1 else h3))
    whatis(extract(if (true) h2 else h3))

    whatis(extract(Nil))
    whatis(extract(Vector()))
    whatis(extract(Map[Int,Int]()))
    whatis(extract(Set[Int]()))
    whatis(extract(Seq[Int]()))
    whatis(extract(Array[Int]()))
    whatis(extract(scala.collection.immutable.BitSet(1)))
    whatis(extract("abc"))
    whatis(extract(if (true) Stream(1) else List(1)))
    whatis(extract(if (true) Seq(1) else Set(1)))
  }
}
