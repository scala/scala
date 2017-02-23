trait DSL { def foo: Int }
trait Impl { def foo = 7; def apply(): Any; def result = { println(apply()) } }

object Test extends App {
  def OptiML[R](b: => R) = new Scope[DSL, Impl, R](b)
  OptiML { println("foo: " + foo); 10 }
}