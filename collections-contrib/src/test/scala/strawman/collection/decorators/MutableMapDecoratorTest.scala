package strawman
package collection
package decorators

import org.junit.{Assert, Test}

class MutableMapDecoratorTest {

  @Test
  def updateWith(): Unit = {
    val ms = new MultiSet[String]
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

  class MultiSet[A] {
    val elems = mutable.Map.empty[A, Int]
    def += (elem: A): this.type = {
      elems.updateWith(elem) { case Some(n) => Some(n + 1) case None => Some(1) }
      this
    }
    def -= (elem: A): this.type = {
      elems.updateWith(elem) { case Some(n) => if (n > 1) Some(n - 1) else None }
      this
    }
  }

}
