object Test extends scala.tools.partest.ReplTest {

  override def transformSettings(settings: scala.tools.nsc.Settings) = {
    settings.noimports.value = true
    settings.nopredef.value = true
    settings
  }

  // replace indylambda function names by <function0>
  override def normalize(s: String) = """\$\$Lambda.*""".r.replaceAllIn(s, "<function0>")

  def code = """
1
1.0
()
"abc"
(1, 2)

{ import scala.Predef.ArrowAssoc; 1 -> 2 }
{ import scala.Predef.ArrowAssoc; 1 → 2 }
1 -> 2
1 → 2

val answer = 42
{ import scala.StringContext; s"answer: $answer" }
s"answer: $answer"

"abc" + true

{ import scala.Predef.any2stringadd; true + "abc" }
true + "abc"

var x = 10
var y = 11
x = 12
y = 13

2 ; 3
{ 2 ; 3 }
5 ; 10 ; case object Cow ; 20 ; class Moo { override def toString = "Moooooo" } ; 30 ; def
bippy = {
  1 +
  2 +
  3 } ; bippy+88+11

object Bovine { var x: scala.List[_] = null } ; case class Ruminant(x: scala.Int) ; bippy * bippy * bippy
Bovine.x = scala.List(Ruminant(5), Cow, new Moo)
Bovine.x

(2)
(2 + 2)
((2 + 2))
  ((2 + 2))
  (  (2 + 2))
  (  (2 + 2 )  )
5 ;   (  (2 + 2 )  ) ; ((5))
(((2 + 2)), ((2 + 2)))
(((2 + 2)), ((2 + 2)), 2)
(((((2 + 2)), ((2 + 2)), 2).productIterator ++ scala.Iterator(3)).mkString)

55 ; ((2 + 2)) ; (1, 2, 3)
55 ; (x: scala.Int) => x + 1 ; () => ((5))

() => 5
55 ; () => 5
() => { class X ; new X }

def foo(x: scala.Int)(y: scala.Int)(z: scala.Int) = x+y+z
foo(5)(10)(15)+foo(5)(10)(15)

scala.List(1) ++ scala.List('a')

:paste < EOF
class C { def c = 42 }
EOF
new C().c
:paste <| EOF
class D { def d = 42 }
EOF
new D().d

:paste < EOF
class Dingus
{
  private val x = 5
  def y = Dingus.x * 2
}
object Dingus
{
  private val x = 55
}
EOF
val x = (new Dingus).y

val x1 = 1
val x2 = 2
val x3 = 3
case class BippyBungus()
x1 + x2 + x3
:reset
x1 + x2 + x3
val x1 = 4
new BippyBungus
class BippyBungus() { def f = 5 }
{ new BippyBungus ; x1 }

"""
}
