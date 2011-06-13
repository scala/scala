import scala.tools.partest.ReplTest

object Test extends ReplTest {
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
((((2 + 2)), ((2 + 2)), 2).productIterator ++ Iterator(3) mkString)

55 ; ((2 + 2)) ; (1, 2, 3)

() => 5
55 ; () => 5
() => { class X ; new X }

def foo(x: Int)(y: Int)(z: Int) = x+y+z
foo(5)(10)(15)+foo(5)(10)(15)

  """.trim
}