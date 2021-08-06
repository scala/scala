package scala

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

import scala.tools.testkit.RunTesting

class OptionTest extends RunTesting {
  @Test def testSomeZipSome(): Unit = assertEquals(Some("foo") zip Some("bar"), Some(("foo", "bar")))
  @Test def testSomeZipNone(): Unit = assertEquals(Some("foo") zip None, None)
  @Test def testNoneZipSome(): Unit = assertEquals(None zip Some("bar"), None)
  @Test def testNoneZipNone(): Unit = assertEquals(None zip None, None)

  @Test def testSomeUnzipToSomePair(): Unit = assertEquals(Some(("foo", "bar")).unzip, (Some("foo"), Some("bar")))
  @Test def testSomeUnzipToSomeNone(): Unit = assertEquals(Some(("foo", null)).unzip, (Some("foo"), Some(null)))
  @Test def testNoneUnzipToNonePair(): Unit = assertEquals(None.unzip, (None, None))

  @Test def testSomeUnzip3ToSomeTriple(): Unit = assertEquals(Some(("foo", "bar", "z")).unzip3, (Some("foo"), Some("bar"), Some("z")))
  @Test def testSomeUnzip3ToSomeNone(): Unit = assertEquals(Some(("foo", null, null)).unzip3, (Some("foo"), Some(null), Some(null)))
  @Test def testNoneUnzip3ToNoneTriple(): Unit = assertEquals(None.unzip3, (None, None, None))

  import runner._
  import scala.tools.reflect.{ ToolBoxError => E }

  @Test() def testSomeZipList(): Unit = assertThrows(classOf[E], run("""Some("foo") zip List("bar", "baz")"""))
  @Test() def testSomeZipNil(): Unit = assertThrows(classOf[E], run("""Some("foo") zip Nil"""))
  @Test() def testNoneZipList(): Unit = assertThrows(classOf[E], run("""None zip List("bar")"""))
  @Test() def testNoneZipNil(): Unit = assertThrows(classOf[E], run("""None zip Nil"""))
}
