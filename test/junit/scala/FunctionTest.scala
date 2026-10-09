package scala

import org.junit.Assert._
import org.junit.Test

class FunctionTest {

  case class Config(name: String)
  case class Logger(prefix: String)
  case class Service(config: Config, logger: Logger)

  @Test
  def `Function0 applyToContext applies the function`(): Unit = {
    var count = 0
    val f = () => { count += 1; "result" }
    assertEquals("result", f.applyToContext)
    assertEquals(1, count)
  }

  @Test
  def `Function1 applyToContext takes the argument from the implicit context`(): Unit = {
    implicit val config: Config = Config("prod")
    val f = (c: Config) => c.name.length
    assertEquals(4, f.applyToContext)
  }

  @Test
  def `Function1 applyToContext accepts an explicit argument`(): Unit = {
    val f = (c: Config) => c.name.length
    assertEquals(4, f.applyToContext(Config("test")))
  }

  @Test
  def `Function1 applyToContext works for specialized functions`(): Unit = {
    implicit val i: Int = 20
    val f = (x: Int) => x + 1
    assertEquals(21, f.applyToContext)
  }

  @Test
  def `Function2 applyToContext takes all arguments from the implicit context`(): Unit = {
    implicit val config: Config = Config("prod")
    implicit val logger: Logger = Logger("[app]")
    val service = (Service.apply _).applyToContext
    assertEquals(Service(config, logger), service)
  }

  @Test
  def `Function3 applyToContext passes the arguments in order`(): Unit = {
    implicit val i: Int = 1
    implicit val s: String = "a"
    implicit val c: Char = 'b'
    val f = (x: Int, y: String, z: Char) => s"$x$y$z"
    assertEquals("1ab", f.applyToContext)
  }

  @Test
  def `Function22 applyToContext takes all arguments from the implicit context`(): Unit = {
    class C(val i: Int)
    implicit val c: C = new C(1)
    val f = (
      a1: C, a2: C, a3: C, a4: C, a5: C, a6: C, a7: C, a8: C, a9: C, a10: C, a11: C,
      a12: C, a13: C, a14: C, a15: C, a16: C, a17: C, a18: C, a19: C, a20: C, a21: C, a22: C) =>
      List(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22).map(_.i).sum
    assertEquals(22, f.applyToContext)
  }

  @Test
  def `Function.applyToContext takes the arguments from the implicit context`(): Unit = {
    implicit val config: Config = Config("prod")
    implicit val logger: Logger = Logger("[app]")
    assertEquals("x", Function.applyToContext(() => "x"))
    assertEquals(config, Function.applyToContext((c: Config) => c))
    assertEquals(Service(config, logger), Function.applyToContext(Service.apply _))
  }

  @Test
  def `applyToContext wires components declared in any order`(): Unit = {
    object components {
      implicit lazy val service: Service = (Service.apply _).applyToContext
      implicit lazy val logger: Logger = Logger(config.name)
      implicit lazy val config: Config = Config("prod")
    }
    assertEquals(Service(Config("prod"), Logger("prod")), components.service)
  }
}
