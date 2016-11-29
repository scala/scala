package scala.collection

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

object TraversableLikeTest {
  abstract class FakeIndexedSeq[A] extends IndexedSeq[A] {
    def apply(i: Int): A = ???
    def length: Int = 0
  }
}

@RunWith(classOf[JUnit4])
class TraversableLikeTest {
  import TraversableLikeTest._

  // For test_SI9019; out here because as of test writing, putting this in a method would crash compiler
  class Baz[@specialized(Int) A]() extends IndexedSeq[A] {
    def apply(i: Int) = ???
    def length: Int = 0
  }

  @Test
  def test_SI9019 {
    object Foo {
      def mkBar = () => {
        class Bar extends FakeIndexedSeq[Int]
        new Bar
      }

      def mkFalsePositiveToSyntheticTest = () => {
        /* A class whose name tarts with an ASCII lowercase letter.
         * It will be a false positive to the synthetic-part test.
         */
        class falsePositive extends FakeIndexedSeq[Int]
        new falsePositive
      }

      def mkFrench = () => {
        // For non-French speakers, this means "strange class name"
        class ÉtrangeNomDeClasse extends FakeIndexedSeq[Int]
        new ÉtrangeNomDeClasse
      }

      def mkFrenchLowercase = () => {
        class étrangeNomDeClasseMinuscules extends FakeIndexedSeq[Int]
        new étrangeNomDeClasseMinuscules
      }
    }

    val bar = Foo.mkBar()
    assertEquals("Bar", bar.stringPrefix)  // Previously would have been outermost class, TraversableLikeTest

    val baz = new Baz[Int]()
    assertEquals("TraversableLikeTest.Baz", baz.stringPrefix)  // Make sure we don't see specialization $mcI$sp stuff

    // The false positive unfortunately produces an empty stringPrefix
    val falsePositive = Foo.mkFalsePositiveToSyntheticTest()
    assertEquals("", falsePositive.stringPrefix)

    val french = Foo.mkFrench()
    assertEquals("ÉtrangeNomDeClasse", french.stringPrefix)

    val frenchLowercase = Foo.mkFrenchLowercase()
    assertEquals("étrangeNomDeClasseMinuscules", frenchLowercase.stringPrefix)
  }
}
