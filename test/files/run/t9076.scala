import scala.tools.partest.SessionTest

object Test extends SessionTest {
  /* avoid objects that don't override toString
  // was: timeout
  def code = """
    |import concurrent._,duration._,ExecutionContext.Implicits._
    |val y = Future { 9 } ; val z = Future { 7 } ; val r = Await.result(for (a <- y; b <- z) yield (a+b), 5.seconds)
  """.stripMargin
  */
  // was: hang
  def session =
s"""|Type in expressions to have them evaluated.
    |Type :help for more information.
    |
    |scala> def i = 42 ; List(1, 2, 3).par.map(x => x + i)
    |i: Int
    |res0: scala.collection.parallel.immutable.ParSeq[Int] = ParVector(43, 44, 45)
    |
    |scala> :quit
 """.stripMargin.trim
}
