package scala

import org.junit.Assert._
import org.junit.Test

import scala.tools.testing.RunTesting

class OptionTest extends RunTesting {
  @Test def testSomeZipSome: Unit = assertEquals(Some("foo") zip Some("bar"), Some(("foo", "bar")))
  @Test def testSomeZipNone: Unit = assertEquals(Some("foo") zip None, None)
  @Test def testNoneZipSome: Unit = assertEquals(None zip Some("bar"), None)
  @Test def testNoneZipNone: Unit = assertEquals(None zip None, None)

  import runner._
  import scala.tools.reflect.{ ToolBoxError => E }

  @Test(expected = classOf[E]) def testSomeZipList: Unit = run("""Some("foo") zip List("bar", "baz")""")
  @Test(expected = classOf[E]) def testSomeZipNil: Unit = run("""Some("foo") zip Nil""")
  @Test(expected = classOf[E]) def testNoneZipList: Unit = run("""None zip List("bar")""")
  @Test(expected = classOf[E]) def testNoneZipNil: Unit = run("""None zip Nil""")
}
