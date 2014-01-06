import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
    |:type List[1, 2, 3]
    |:type List(1, 2, 3)
    |:type def foo[T](x: T) = List(x)
    |:type val bar = List(Set(1))
    |:type lazy val bar = Set(Set(1))
    |:type def f[T >: Null, U <: String](x: T, y: U) = Set(x, y)
    |:type def x = 1 ; def bar[T >: Null <: AnyRef](xyz: T) = 5
    |
    |:type 5
    |:type val f = 5
    |:type lazy val f = 5
    |:type protected lazy val f = 5
    |:type def f = 5
    |:type def f() = 5
    |
    |:type def g[T](xs: Set[_ <: T]) = Some(xs.head)
    |
    |// verbose!
    |:type -v List(1,2,3) filter _
    |:type -v def f[T >: Null, U <: String](x: T, y: U) = Set(x, y)
    |:type -v def x = 1 ; def bar[T >: Null <: AnyRef](xyz: T) = 5
    |:type -v Nil.combinations _
    |:type -v def f[T <: AnyVal] = List[T]().combinations _
    |:type -v def f[T, U >: T](x: T, y: List[U]) = x :: y
    |
    |// SI-7132 - :type doesn't understand Unit
    |:type ()
    |:type println("side effect!")
  """.stripMargin
}

