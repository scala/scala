import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
    |// verbose!
    |:type -v def f = 5
    |:type -v def f() = 5
    |:type -v def f[T] = 5
    |:type -v def f[T >: Null] = 5
    |:type -v def f[T <: String] = 5
    |:type -v def f[T]() = 5
    |:type -v def f[T, U]() = 5
    |:type -v def f[T, U]()() = 5
    |:type -v def f[T, U <: T] = 5
    |:type -v def f[T, U <: T](x: T)(y: U) = 5
    |:type -v def f[T: Ordering] = 5
    |:type -v def f[T: Ordering] = implicitly[Ordering[T]]
    |:type -v def f[T <: { type Bippy = List[Int] ; def g(): Bippy }] = 5
  """.stripMargin
}
