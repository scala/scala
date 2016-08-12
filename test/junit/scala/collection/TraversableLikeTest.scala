package scala.collection

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class TraversableLikeTest {
  // For test_SI9019; out here because as of test writing, putting this in a method would crash compiler
  class Baz[@specialized(Int) A]() extends IndexedSeq[A] {
    def apply(i: Int) = ???
    def length: Int = 0
  }
  
  @Test
  def test_SI9019 {
    object Foo {
      def mkBar = () => {
        class Bar extends IndexedSeq[Int] {
          def apply(i: Int) = ???
          def length: Int = 0
        }
        new Bar
      }
    }
    val bar = Foo.mkBar()
    assertEquals("Bar", bar.stringPrefix)  // Previously would have been outermost class, TraversableLikeTest

    val baz = new Baz[Int]()
    assertEquals("TraversableLikeTest.Baz", baz.stringPrefix)  // Make sure we don't see specialization $mcI$sp stuff
  }
}
