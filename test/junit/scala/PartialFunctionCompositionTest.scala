package scala

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class PartialFunctionCompositionTest {
  val fallbackFun = (_: String) => "fallback"
  val passAll: PartialFunction[String, String] = { case any => any }
  val passShort: PartialFunction[String, String] = { case any if any.length < 5 => any }
  val passPass: PartialFunction[String, String] = { case s if s.contains("pass") => s }

  val allAndThenShort = passAll andThen passShort
  val shortAndThenAll = passShort andThen passAll
  val allAndThenPass = passAll andThen passPass
  val passAndThenAll = passPass andThen passAll
  val passAndThenShort = passPass andThen passShort
  val shortAndThenPass = passShort andThen passPass

  @Test
  def andThenTests(): Unit = {
    assertEquals(allAndThenShort.applyOrElse("pass", fallbackFun), "pass")
    assertEquals(shortAndThenAll.applyOrElse("pass", fallbackFun), "pass")
    assertTrue(allAndThenShort.isDefinedAt("pass"))
    assertTrue(shortAndThenAll.isDefinedAt("pass"))

    assertEquals(allAndThenPass.applyOrElse("pass", fallbackFun), "pass")
    assertEquals(passAndThenAll.applyOrElse("pass", fallbackFun), "pass")
    assertTrue(allAndThenPass.isDefinedAt("pass"))
    assertTrue(passAndThenAll.isDefinedAt("pass"))

    assertEquals(allAndThenPass.applyOrElse("longpass", fallbackFun), "longpass")
    assertEquals(passAndThenAll.applyOrElse("longpass", fallbackFun), "longpass")
    assertTrue(allAndThenPass.isDefinedAt("longpass"))
    assertTrue(passAndThenAll.isDefinedAt("longpass"))

    assertEquals(allAndThenShort.applyOrElse("longpass", fallbackFun), "fallback")
    assertEquals(shortAndThenAll.applyOrElse("longpass", fallbackFun), "fallback")
    assertFalse(allAndThenShort.isDefinedAt("longpass"))
    assertFalse(shortAndThenAll.isDefinedAt("longpass"))

    assertEquals(allAndThenPass.applyOrElse("longstr", fallbackFun), "fallback")
    assertEquals(passAndThenAll.applyOrElse("longstr", fallbackFun), "fallback")
    assertFalse(allAndThenPass.isDefinedAt("longstr"))
    assertFalse(passAndThenAll.isDefinedAt("longstr"))

    assertEquals(passAndThenShort.applyOrElse("pass", fallbackFun), "pass")
    assertEquals(shortAndThenPass.applyOrElse("pass", fallbackFun), "pass")
    assertTrue(passAndThenShort.isDefinedAt("pass"))
    assertTrue(shortAndThenPass.isDefinedAt("pass"))

    assertEquals(passAndThenShort.applyOrElse("longpass", fallbackFun), "fallback")
    assertEquals(shortAndThenPass.applyOrElse("longpass", fallbackFun), "fallback")
    assertFalse(passAndThenShort.isDefinedAt("longpass"))
    assertFalse(shortAndThenPass.isDefinedAt("longpass"))

    assertEquals(shortAndThenPass.applyOrElse("longstr", fallbackFun), "fallback")
    assertEquals(passAndThenShort.applyOrElse("longstr", fallbackFun), "fallback")
    assertFalse(shortAndThenPass.isDefinedAt("longstr"))
    assertFalse(passAndThenShort.isDefinedAt("longstr"))
  }

  val allComposeShort = passAll compose passShort
  val shortComposeAll = passShort compose passAll
  val allComposePass = passAll compose passPass
  val passComposeAll = passPass compose passAll
  val passComposeShort = passPass compose passShort
  val shortComposePass = passShort compose passPass

  @Test
  def composeTests(): Unit = {
    assertEquals(allComposeShort.applyOrElse("pass", fallbackFun), "pass")
    assertEquals(shortComposeAll.applyOrElse("pass", fallbackFun), "pass")
    assertTrue(allComposeShort.isDefinedAt("pass"))
    assertTrue(shortComposeAll.isDefinedAt("pass"))

    assertEquals(allComposePass.applyOrElse("pass", fallbackFun), "pass")
    assertEquals(passComposeAll.applyOrElse("pass", fallbackFun), "pass")
    assertTrue(allComposePass.isDefinedAt("pass"))
    assertTrue(passComposeAll.isDefinedAt("pass"))

    assertEquals(allComposePass.applyOrElse("longpass", fallbackFun), "longpass")
    assertEquals(passComposeAll.applyOrElse("longpass", fallbackFun), "longpass")
    assertTrue(allComposePass.isDefinedAt("longpass"))
    assertTrue(passComposeAll.isDefinedAt("longpass"))

    assertEquals(allComposeShort.applyOrElse("longpass", fallbackFun), "fallback")
    assertEquals(shortComposeAll.applyOrElse("longpass", fallbackFun), "fallback")
    assertFalse(allComposeShort.isDefinedAt("longpass"))
    assertFalse(shortComposeAll.isDefinedAt("longpass"))

    assertEquals(allComposePass.applyOrElse("longstr", fallbackFun), "fallback")
    assertEquals(passComposeAll.applyOrElse("longstr", fallbackFun), "fallback")
    assertFalse(allComposePass.isDefinedAt("longstr"))
    assertFalse(passComposeAll.isDefinedAt("longstr"))

    assertEquals(passComposeShort.applyOrElse("pass", fallbackFun), "pass")
    assertEquals(shortComposePass.applyOrElse("pass", fallbackFun), "pass")
    assertTrue(passComposeShort.isDefinedAt("pass"))
    assertTrue(shortComposePass.isDefinedAt("pass"))

    assertEquals(passComposeShort.applyOrElse("longpass", fallbackFun), "fallback")
    assertEquals(shortComposePass.applyOrElse("longpass", fallbackFun), "fallback")
    assertFalse(passComposeShort.isDefinedAt("longpass"))
    assertFalse(shortComposePass.isDefinedAt("longpass"))

    assertEquals(shortComposePass.applyOrElse("longstr", fallbackFun), "fallback")
    assertEquals(passComposeShort.applyOrElse("longstr", fallbackFun), "fallback")
    assertFalse(shortComposePass.isDefinedAt("longstr"))
    assertFalse(passComposeShort.isDefinedAt("longstr"))
  }

  @Test
  def andThenWithTotalFunctionTests(): Unit = {
    val f = (s: String) => s"$s$s"
    val pf: PartialFunction[String, String] = { case any if any.length < 5 => any }

    assertEquals((pf andThen f).applyOrElse("pass", fallbackFun), "passpass")
    assertEquals((pf andThen f).applyOrElse("passpass", fallbackFun), "fallback")
  }

  @Test
  def andThenWithUpcastPartialFunctionTests(): Unit = {
    val f: PartialFunction[Int, Int] = {case x if x % 2 == 0 => x + 2}
    val g: PartialFunction[Int, Int] = {case x if x % 2 == 1 => x - 2}
    val c1 = f andThen g
    val c2 = f andThen (g: Int => Int)
    assertEquals(8, c1.applyOrElse(2, (_: Int) => 8))
    assertEquals(8, c2.applyOrElse(2, (_: Int) => 8))
  }

  @Test
  def inferenceTests(): Unit = {
    val fb = (_: Int) => 42
    val pf: PartialFunction[Int, Int] = { case x if x > 10 => x + 1 }
    val pf2 = pf andThen (x => x + 1)

    assertEquals(pf2.applyOrElse(11, fb), 13)
    assertFalse(pf2.isDefinedAt(10))

    val pf3 = pf andThen { case x if x > 20 => x + 1 }
    assertEquals(pf3.applyOrElse(15, fb), 42)
    assertEquals(pf3.applyOrElse(21, fb), 23)
    assertFalse(pf3.isDefinedAt(15))
    assertTrue(pf3.isDefinedAt(21))
  }
}
