trait P { def reflected: Boolean }
trait Q { def reflected: Boolean }
class PQ(val reflected: Boolean) extends P with Q { }

trait A
trait B
trait C { val y: P }
class ABC extends A with B with C {
  private def reflected = (
    Thread.currentThread.getStackTrace
      takeWhile (_.getMethodName != "main")
      exists (_.toString contains "sun.reflect.")
  )
  lazy val y: PQ = new PQ(reflected)
}

/*** The source used to generate the second file
     Not otherwise used in the test except that compiling
     it helps make sure it still compiles.

****/

object Gen {
  case class Tp(outer: String, elem: String) {
    override def toString = s"$outer { val y: $elem }"
  }
  case class Pair(tp1: Tp, tp2: Tp) {
    def expr = s"((new ABC): $tp)"
    def tp   = s"($tp1) with ($tp2)"
  }
  val traits = Vector("Any", "A", "B", "C") map ("%6s" format _)
  val types  = Vector("P", "Q", "R forSome { type R <: P with Q }")
  val allTypes = for (c <- traits ; tp <- types) yield Tp(c, tp)
  val pairs = allTypes flatMap (t1 => allTypes map (t2 => Pair(t1, t2)))
  val indices = pairs.indices

  def aliases(idx: Int) = {
    val p = pairs(idx)
    import p._
    List(
      s"type R1_$idx = $tp",
      s"type R2_$idx = R1_$idx { val y: (${tp1.elem}) with (${tp2.elem}) }"
    )
  }

  def mkMethodContent(pre: String)(f: Int => String) =
    indices map (i => s"def $pre$i${f(i)}") mkString "\n  "

  def content = List(
    indices flatMap aliases mkString "\n  ",
    mkMethodContent("f")(i => s" = { val x = ${pairs(i).expr} ; x.y.reflected -> whatis(x).toString }"),
    mkMethodContent("g")(i => s"""(x: R1_$i) = x.y"""),
    mkMethodContent("h")(i => s"""(x: R2_$i) = x.y""")
  ) mkString "\n  "

  def fCalls = indices map ("f" + _) mkString ("\n    ", ",\n    ", "\n  ")

  def main(args: Array[String]): Unit = {
    // One cannot attain proper appreciation for the inadequacies of
    // string interpolation without becoming one with the newline.
    val nl = "\\n"

    println(s"""
      |import scala.reflect.runtime.universe._
      |import scala.language._
      |
      |object Test {
      |  def whatis[T: TypeTag](x: T) = typeOf[T]
      |  def sshow(label: String, xs: Traversable[Any]) {
      |    println("==== " + label + " ====$nl")
      |    xs.toList.map("" + _).sorted foreach println
      |    println("$nl")
      |  }
      |
      |  $content
      |  lazy val fcalls = List($fCalls)
      |
      |  def main(args: Array[String]) {
      |    sshow("Direct Calls", fcalls collect { case (false, n) => n })
      |    sshow("Reflective Calls", fcalls collect { case (true, n) => n })
      |    // For a good time try printing this - have to fix bugs in
      |    // reflection before that's going to be a good idea
      |    // println(typeOf[Test.type].typeSymbol.asClass.info)
      |  }
      |}
      """.stripMargin.trim
    )
  }
}
