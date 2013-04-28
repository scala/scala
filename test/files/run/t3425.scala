import scala.language.reflectiveCalls
object Other {
  abstract class Foo {
    type R1 <:    { def x: Any }
    type R2 <: R1 { def x: Int }

    def f(x: R2) = x.x
  }

  abstract class Bar {
    trait R0      { def x: Any }
    type R1 <: R0 { def x: AnyVal }
    type R2 <: R1 { def x: Int }

    def f(x: R2) = x.x
  }
}
object Test {
  trait A
  trait B
  def x(a: (A { val y: Int }) with B { val y: Int }) = a.y

  class C extends A with B {
    val y = 456
  }

  class Bippy { def x: Int = 789 }

  def main(args: Array[String]): Unit = {
    println(x(new A with B { val y = 123 }))
    println(x(new C))

    { val foo = new Other.Foo { type R1 = Bippy ; type R2 = Bippy }
      println(foo.f(new Bippy))
    }
    { val bar = new Other.Bar { type R1 = Bippy with R0 ; type R2 = R1 }
      println(bar.f(new Bippy with bar.R0))
    }
  }
}

