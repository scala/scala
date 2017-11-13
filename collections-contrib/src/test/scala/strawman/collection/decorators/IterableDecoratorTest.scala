package strawman.collection
package decorators

import org.junit.{Assert, Test}
import strawman.collection.immutable.{List, Range}

class IterableDecoratorTest {

  @Test
  def foldSomeLeft(): Unit = {
      val r = Range(0, 100)
      Assert.assertEquals(0, r.foldSomeLeft(0)((x, y) => None))
      Assert.assertEquals(10, r.foldSomeLeft(0)((x, y) => if (y > 10) None else Some(y)))
      Assert.assertEquals(55, r.foldSomeLeft(0)((x, y) => if (y > 10) None else Some(x + y)))
      Assert.assertEquals(4950, r.foldSomeLeft(0)((x, y) => Some(x + y)))

      Assert.assertEquals(10, List[Int]().foldSomeLeft(10)((x, y) => Some(x + y)))
    }

}
