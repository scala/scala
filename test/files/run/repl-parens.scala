import scala.tools.partest.ReplTest

object Test extends ReplTest {
  // replace indylambda function names by <function0>
  override def normalize(s: String) = """\$\$Lambda.*""".r.replaceAllIn(s, "<function0>")

  def code = """
(2)
(2 + 2)
((2 + 2))
  ((2 + 2))
  (  (2 + 2))
  (  (2 + 2 )  )
5 ;   (  (2 + 2 )  ) ; ((5))
(((2 + 2)), ((2 + 2)))
(((2 + 2)), ((2 + 2)), 2)
(((((2 + 2)), ((2 + 2)), 2).productIterator ++ Iterator(3)).mkString)

55 ; ((2 + 2)) ; (1, 2, 3)
55 ; (x: Int) => x + 1 ; () => ((5))

() => 5
55 ; () => 5
() => { class X ; new X }

def foo(x: Int)(y: Int)(z: Int) = x+y+z
foo(5)(10)(15)+foo(5)(10)(15)

List(1) ++ List('a')

  """.trim
}
