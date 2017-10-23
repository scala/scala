package strawman
package collection
package decorators

import org.junit.{Assert, Test}

class ImmutableMapDecoratorTest {

  @Test
  def updatedWith(): Unit = {
    var ms = new MultiSet[String](immutable.Map.empty)
    ms -= "foo"
    Assert.assertEquals(Map.empty, ms.elems)
    ms += "foo"
    ms += "bar"
    ms += "foo"
    Assert.assertEquals(Map("foo" -> 2, "bar" -> 1), ms.elems)
    ms -= "foo"
    ms -= "bar"
    Assert.assertEquals(Map("foo" -> 1), ms.elems)
  }

  class MultiSet[A](val elems: immutable.Map[A, Int]) {
    def + (elem: A): MultiSet[A] =
      new MultiSet(elems.updatedWith(elem) { case Some(n) => Some(n + 1) case None => Some(1) })
    def - (elem: A): MultiSet[A] =
      new MultiSet(elems.updatedWith(elem) { case Some(n) => if (n > 1) Some(n - 1) else None })
  }

}
