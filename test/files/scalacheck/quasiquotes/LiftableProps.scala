import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

import scala.reflect.runtime.universe._
import Flag._

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
}