import scala.tools.partest.ReplTest
import scala.tools.nsc.Settings

object Test extends ReplTest {

  override def transformSettings(s: Settings): Settings = {
    s.Yreplclassbased.value = true
    s
  }

  override def normalize(s: String) = {
    // replace indylambda function names by <function0>
    """\$Lambda.*""".r.replaceAllIn(s, "<function0>")
  }

  def code = """
    |var x = 10
    |var y = 11
    |x = 12
    |y = 13
    |val z = x * y
    |2 ; 3
    |{ 2 ; 3 }
    |5 ; 10 ; case object Cow ; 20 ; class Moo { override def toString = "Moooooo" } ; 30 ; def bippy = {
    |  1 +
    |  2 +
    |  3 } ; bippy+88+11
    |
    |object Bovine { var x: List[_] = null } ; case class Ruminant(x: Int) ; bippy * bippy * bippy
    |Bovine.x = List(Ruminant(5), Cow, new Moo)
    |Bovine.x
    |
    |(2)
    |(2 + 2)
    |((2 + 2))
    |  ((2 + 2))
    |  (  (2 + 2))
    |  (  (2 + 2 )  )
    |5 ;   (  (2 + 2 )  ) ; ((5))
    |(((2 + 2)), ((2 + 2)))
    |(((2 + 2)), ((2 + 2)), 2)
    |(((((2 + 2)), ((2 + 2)), 2).productIterator ++ Iterator(3)).mkString)
    |
    |55 ; ((2 + 2)) ; (1, 2, 3)
    |55 ; (x: Int) => x + 1 ; () => ((5))
    |
    |() => 5
    |55 ; () => 5
    |() => { class X ; new X }
    |
    |def foo(x: Int)(y: Int)(z: Int) = x+y+z
    |foo(5)(10)(15)+foo(5)(10)(15)
    |
    |List(1) ++ List('a')
    |
    |1 to 100 map (_  + 1)
    |val x1 = 1
    |val x2 = 2
    |val x3 = 3
    |case class BippyBungus()
    |x1 + x2 + x3
    |:reset
    |x1 + x2 + x3
    |val x1 = 4
    |new BippyBungus
    |class BippyBungus() { def f = 5 }
    |{ new BippyBungus ; x1 }
    |object x {class y { case object z } }
    |case class BippyBups()
    |case class PuppyPups()
    |case class Bingo()
    |List(BippyBups(), PuppyPups(), Bingo()) // show
    |case class Sum(exp: String, exp2: String)
    |val a = Sum("A", "B")
    |def b(a: Sum): String = a match { case Sum(_, _) => "Found Sum" }
    |b(a)
    |:power
    |intp.lastRequest
    |""".stripMargin
}
