import org.scalacheck._, Prop._, Gen._, Arbitrary._
import scala.reflect.runtime.universe._, Flag._

object LiftableProps extends QuasiquoteProperties("liftable") {
  property("splice byte") = test {
    val c: Byte = 0
    assert(q"$c" ≈ Literal(Constant(c)))
  }

  property("splice short") = test {
    val c: Short = 0
    assert(q"$c" ≈ Literal(Constant(c)))
  }

  property("splice char") = test {
    val c: Char = 'c'
    assert(q"$c" ≈ Literal(Constant(c)))
  }

  property("splice int") = test {
    val c: Int = 0
    assert(q"$c" ≈ Literal(Constant(c)))
  }

  property("splice long") = test {
    val c: Long = 0
    assert(q"$c" ≈ Literal(Constant(c)))
  }

  property("splice float") = test {
    val c: Float = 0.0f
    assert(q"$c" ≈ Literal(Constant(c)))
  }

  property("splice double") = test {
    val c: Double = 0.0
    assert(q"$c" ≈ Literal(Constant(c)))
  }

  property("splice boolean") = test {
    val c: Boolean = false
    assert(q"$c" ≈ Literal(Constant(c)))
  }

  property("splice string") = test {
    val c: String = "s"
    assert(q"$c" ≈ Literal(Constant(c)))
  }

  property("splice unit") = test {
    val c: Unit = ()
    assert(q"$c" ≈ Literal(Constant(c)))
  }

  property("lift symbol") = test {
    val s = rootMirror.staticClass("scala.Int")
    assert(q"$s" ≈ Ident(s))
  }

  property("lift type") = test {
    val tpe = rootMirror.staticClass("scala.Int").toType
    assert(q"$tpe" ≈ TypeTree(tpe))
  }

  property("lift type tag") = test {
    val tag = TypeTag.Int
    assert(q"$tag" ≈ TypeTree(tag.tpe))
  }

  property("lift weak type tag") = test {
    val tag = WeakTypeTag.Int
    assert(q"$tag" ≈ TypeTree(tag.tpe))
  }

  property("lift constant") = test {
    val const = Constant(0)
    assert(q"$const" ≈ q"0")
  }

  property("lift list variants") = test {
    val lst = List(1, 2)
    val immutable = q"$scalapkg.collection.immutable"
    assert(q"$lst" ≈ q"$immutable.List(1, 2)")
    assert(q"f(..$lst)" ≈ q"f(1, 2)")
    val llst = List(List(1), List(2))
    assert(q"f(..$llst)" ≈ q"f($immutable.List(1), $immutable.List(2))")
    assert(q"f(...$llst)" ≈ q"f(1)(2)")
  }

  property("lift tuple") = test {
    assert(q"${Tuple1(1)}" ≈ q"scala.Tuple1(1)")
    assert(q"${(1, 2)}" ≈ q"(1, 2)")
    assert(q"${(1, 2, 3)}" ≈ q"(1, 2, 3)")
    assert(q"${(1, 2, 3, 4)}" ≈ q"(1, 2, 3, 4)")
    assert(q"${(1, 2, 3, 4, 5)}" ≈ q"(1, 2, 3, 4, 5)")
    assert(q"${(1, 2, 3, 4, 5, 6)}" ≈ q"(1, 2, 3, 4, 5, 6)")
    assert(q"${(1, 2, 3, 4, 5, 6, 7)}" ≈ q"(1, 2, 3, 4, 5, 6, 7)")
    assert(q"${(1, 2, 3, 4, 5, 6, 7, 8)}" ≈ q"(1, 2, 3, 4, 5, 6, 7, 8)")
    assert(q"${(1, 2, 3, 4, 5, 6, 7, 8, 9)}" ≈ q"(1, 2, 3, 4, 5, 6, 7, 8, 9)")
    assert(q"${(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)}" ≈ q"(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)")
    assert(q"${(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)}" ≈ q"(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)")
    assert(q"${(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)}" ≈ q"(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)")
    assert(q"${(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)}" ≈ q"(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)")
    assert(q"${(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)}" ≈ q"(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)")
    assert(q"${(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)}" ≈ q"(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)")
    assert(q"${(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)}" ≈ q"(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)")
    assert(q"${(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)}" ≈ q"(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)")
    assert(q"${(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)}" ≈ q"(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)")
    assert(q"${(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19)}" ≈ q"(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19)")
    assert(q"${(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)}" ≈ q"(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)")
    assert(q"${(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)}" ≈ q"(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)")
    assert(q"${(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)}" ≈ q"(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)")
  }
}