import org.scalacheck._, Prop._, Gen._, Arbitrary._
import scala.reflect.runtime.universe._, Flag._, internal.reificationSupport.ScalaDot

object TypeConstructionProps extends QuasiquoteProperties("type construction")  {
  property("bare idents contain type names") = test {
    tq"x" ≈ Ident(TypeName("x"))
  }

  property("unquote type names into AppliedTypeTree") = forAll { (name1: TypeName, name2: TypeName) =>
    tq"$name1[$name2]" ≈ AppliedTypeTree(Ident(name1), List(Ident(name2)))
  }

  property("tuple type") = test {
    val empty = List[Tree]()
    val ts = List(tq"t1", tq"t2")
    assert(tq"(..$empty)" ≈ ScalaDot(TypeName("Unit")))
    assert(tq"(..$ts)" ≈ tq"scala.Tuple2[t1, t2]")
    assert(tq"(t0, ..$ts)" ≈ tq"scala.Tuple3[t0, t1, t2]")
  }

  property("single-element tuple type") = test {
    val ts = q"T" :: Nil
    assert(tq"(..$ts)" ≈ ts.head)
  }

  property("refined type") = test {
    val stats = q"def foo" :: q"val x: Int" :: q"type Y = String" :: Nil
    assert(tq"T { ..$stats }" ≈ tq"T { def foo; val x: Int; type Y = String }")
  }

  property("function type") = test {
    val argtpes = tq"A" :: tq"B" :: Nil
    val restpe = tq"C"
    assert(tq"..$argtpes => $restpe" ≈ tq"(A, B) => C")
  }

  property("empty tq") = test {
    val tt: TypeTree = tq""
    assert(tt.tpe == null)
    assert(tt.original == null)
  }
}
