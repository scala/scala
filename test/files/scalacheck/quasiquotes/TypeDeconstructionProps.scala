import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

import scala.reflect.runtime.universe._
import Flag._

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

  property("tuple type") = test {
    val tq"(..$empty)" = tq"_root_.scala.Unit"
    assert(empty.isEmpty)
    val tq"(..$ts)" = tq"(t1, t2)"
    assert(ts ≈ List(tq"t1", tq"t2"))
    val tq"($head, ..$tail)" = tq"(t0, t1, t2)"
    assert(head ≈ tq"t0" && tail ≈ List(tq"t1", tq"t2"))
  }

  property("refined type") = test {
    val tq"T { ..$stats }" = tq"T { def foo; val x: Int; type Y = String }"
    assert(stats ≈ (q"def foo" :: q"val x: Int" :: q"type Y = String" :: Nil))
  }

  property("function type") = test {
    val tq"..$argtpes => $restpe" = tq"(A, B) => C"
    assert(argtpes ≈ (tq"A" :: tq"B" :: Nil))
    assert(restpe ≈ tq"C")
  }
}