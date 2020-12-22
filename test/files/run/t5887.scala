
import scala.tools.testkit.AssertUtil.assertThrows
import scala.annotation.nowarn

@nowarn("msg=This catches all Throwables.")
object Test extends App {
  def npe: Int = throw null
  def err: Int = throw new Error()

  val pf: PartialFunction[Throwable, Int] = { case _: NullPointerException => 42 }
  val f: Throwable => Int = pf

  assertThrows[NullPointerException](npe)

  assert(42 == (try npe catch pf))
  assert(42 == (try npe catch f))
  assertThrows[Error](try err catch pf)
  assertThrows[MatchError](try err catch f)
}
