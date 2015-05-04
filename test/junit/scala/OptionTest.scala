package scala

import org.junit.Test
import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class OptionTest {
  import Option._

  @Test def someZipSome() = {
    val some = Some("a")
    val other = Some("b")

    val res = some.zip(other)
    assertEquals(res, Some(("a", "b")))
  }

  @Test def someZipNone() = {
    val some = Some("a")
    val none = None

    val res = some.zip(none)
    assertEquals(res, None)
  }

  @Test def noneZipSome() = {
    val some = Some("a")
    val none = None

    val res = none.zip(some)
    assertEquals(res, None)
  }

  @Test def someZipList0() = {
    val some = Some("a")
    val list = List()

    val res = some.zip(list)
    assertEquals(res, None)
  }

  @Test def someZipList1() = {
    val some = Some("a")
    val list = List("a")

    val res = some.zip(list)
    assertEquals(res, Some(("a", "a")))
  }

  @Test def someZipList2() = {
    val some = Some("a")
    val list = List("a", "b")

    val res = some.zip(list)
    assertEquals(res, Some(("a", "a")))
  }

  @Test def noneZipList0() = {
    val none = None
    val list = List()

    val res = none.zip(list)
    assertEquals(res, None)
  }

  @Test def noneZipList1() = {
    val none = None
    val list = List("a")

    val res = none.zip(list)
    assertEquals(res, None)
  }

  @Test def noneZipList2() = {
    val none = None
    val list = List("a", "b")

    val res = none.zip(list)
    assertEquals(res, None)
  }

}