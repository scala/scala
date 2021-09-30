package scala.collection

import org.junit.Assert.{assertNotSame, assertSame}
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.annotation.nowarn
import scala.collection.{immutable => i, mutable => m}
import scala.language.implicitConversions
import scala.{collection => c}

@RunWith(classOf[JUnit4])
@nowarn("cat=deprecation")
class ToConserveTest {
  // scala/bug#12188
  implicit def toAnyRefFactory[A, CC[_] <: AnyRef](factory: c.IterableFactory[CC]): c.Factory[A, AnyRef] =
    c.IterableFactory.toFactory(factory)
  implicit def toFactory[K, V, CC[_, _] <: AnyRef](factory: MapFactory[CC]): Factory[(K, V), AnyRef] =
    c.MapFactory.toFactory(factory)

  @Test def toConserveList: Unit = {
    val l: c.Iterable[Int] = (1 to 3).toList

    assertSame(l, l.toList)
    assertSame(l, l.toSeq)
    assertSame(l, l.toIterable)

    assertSame(l, l.to(c.Iterable))
    assertSame(l, l.to(i.Iterable))

    assertSame(l, l.to(c.Seq))
    assertSame(l, l.to(i.Seq))

    assertSame(l, l.to(c.LinearSeq))
    assertSame(l, l.to(i.LinearSeq))

    assertSame(l, l.to(List))
  }

  @Test def toConserveImmutableHashSet: Unit = {
    val s: c.Iterable[Int] = (1 to 10).to(i.HashSet)
    assertSame(s, s.toSet)
    assertSame(s, s.toIterable)

    assertSame(s, s.to(c.Iterable))
    assertSame(s, s.to(i.Iterable))

    assertSame(s, s.to(c.Set))
    assertSame(s, s.to(i.Set))

    assertSame(s, s.to(i.HashSet))
  }

  @Test def toConserveImmutableHashMap: Unit = {
    val m: c.Iterable[(Int, Int)] = (1 to 10).map(x => (x, x)).to(i.HashMap): i.Map[Int, Int]

    assertSame(m, m.toMap)
    assertSame(m, m.toIterable)

    assertSame(m, m.to(c.Iterable))
    assertSame(m, m.to(i.Iterable))

    assertSame(m, m.to(c.Map))
    assertSame(m, m.to(i.Map))

    assertSame(m, m.to(i.HashMap))
  }

  @Test def toConserveLazyList: Unit = {
    val l: c.Iterable[Int] = LazyList.from(1 to 10)

    assertSame(l, l.toSeq)
    assertSame(l, l.toIterable)

    assertSame(l, l.to(c.Iterable))
    assertSame(l, l.to(i.Iterable))

    assertSame(l, l.to(c.Seq))
    assertSame(l, l.to(i.Seq))

    assertSame(l, l.to(c.LinearSeq))
    assertSame(l, l.to(i.LinearSeq))

    assertSame(l, l.to(LazyList))
  }

  @Test def toRebuildMutable: Unit = {
    val s: c.Iterable[Int] = (1 to 3).to(m.HashSet)
    assertSame(s, s.toIterable) // slightly inconsistent...
    assertNotSame(s, s.to(c.Iterable))
    assertNotSame(s, s.to(m.Iterable))
    assertNotSame(s, s.to(c.Set))
    assertNotSame(s, s.to(m.Set))
    assertNotSame(s, s.to(m.HashSet))

    val b: c.Iterable[Int] = (1 to 6).to(m.ArrayBuffer)
    assertSame(b, b.toIterable) // slightly inconsistent...
    assertNotSame(b, b.toBuffer)
    assertNotSame(b, b.to(c.Iterable))
    assertNotSame(b, b.to(m.Iterable))
    assertNotSame(b, b.to(c.Seq))
    assertNotSame(b, b.to(m.Seq))
    assertNotSame(b, b.to(m.Buffer))
    assertNotSame(b, b.to(m.IndexedBuffer))
    assertNotSame(b, b.to(m.ArrayBuffer))
  }
}
