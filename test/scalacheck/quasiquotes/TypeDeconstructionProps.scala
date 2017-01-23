import org.scalacheck._, Prop._, Gen._, Arbitrary._
import scala.reflect.runtime.universe._, Flag._

object TypeDeconstructionProps extends QuasiquoteProperties("type deconstruction") {
  property("ident(type name)") = forAll { (name: TypeName) =>
    val t = Ident(name)
    val tq"$t1" = t
    t1 ≈ t
  }

  property("applied type tree") = forAll { (name1: TypeName, name2: TypeName) =>
    val tq"$a[$b]" = AppliedTypeTree(Ident(name1), List(Ident(name2)))
    a ≈ Ident(name1) && b ≈ Ident(name2)
  }

  property("tuple type (1)") = test {
    val tq"(..$empty)" = tq"_root_.scala.Unit"
    assert(empty.isEmpty)
  }

  property("tuple type (2)") = test {
    val tq"(..$ts)" = tq"(t1, t2)"
    assert(ts ≈ List(tq"t1", tq"t2"))
  }

  property("tuple type (3)") = test {
    val tq"($head, ..$tail)" = tq"(t0, t1, t2)"
    assert(head ≈ tq"t0")
    assert(tail ≈ List(tq"t1", tq"t2"))
  }

  property("tuple type (4)") = test {
    val tq"(..$init, $last)" = tq"(t0, t1, t2)"
    assert(init ≈ List(tq"t0", tq"t1"))
    assert(last ≈ tq"t2")
  }

  property("tuple type (5)") = test {
    val tq"(..$ts)" = tq"T"
    assert(ts ≈ List(tq"T"))
  }

  property("refined type") = test {
    val tq"T { ..$stats }" = tq"T { def foo; val x: Int; type Y = String }"
    assert(stats ≈ List(q"def foo", q"val x: Int", q"type Y = String"))
  }

  property("function type (1)") = test {
    val tq"..$argtpes => $restpe" = tq"(A, B) => C"
    assert(argtpes ≈ List(tq"A", tq"B"))
    assert(restpe ≈ tq"C")
  }

  property("function type (2)") = test {
    val tq"(..$argtpes, $arglast) => $restpe" = tq"(A, B, C) => D"
    assert(argtpes ≈ List(tq"A", tq"B"))
    assert(arglast ≈ tq"C")
    assert(restpe ≈ tq"D")
  }

  property("match empty type tree") = test {
    val tq"" = TypeTree()
    // matches because type tree isn't syntactic without original
    val tq"" = tq"${typeOf[Int]}"
  }

  property("type select doesn't match term select") = test {
    assertThrows[MatchError] {
      val tq"$qual.$name" = q"foo.bar"
    }
  }

  property("applied type doesn't match type appliction") = test {
    assertThrows[MatchError] {
      val tq"$tpt[..$tpts]" = q"foo[bar]"
    }
  }
}
