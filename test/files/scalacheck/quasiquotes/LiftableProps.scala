import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

import scala.reflect.runtime.universe._
import Flag._

object LiftableProps extends Properties("liftable")
                        with TreeSimiliarity
                        with ArbitraryTreesAndNames {

  // this tests use exists as there is no neeed
  // to test the same property for many possible values

  property("splice byte") = exists { (c: Byte) =>
    q"$c" ≈ Literal(Constant(c))
  }

  property("splice short") = exists { (c: Short) =>
    q"$c" ≈ Literal(Constant(c))
  }

  property("splice char") = exists { (c: Char) =>
    q"$c" ≈ Literal(Constant(c))
  }

  property("splice int") = exists { (c: Int) =>
    q"$c" ≈ Literal(Constant(c))
  }

  property("splice long") = exists { (c: Long) =>
    q"$c" ≈ Literal(Constant(c))
  }

  property("splice float") = exists { (c: Float) =>
    q"$c" ≈ Literal(Constant(c))
  }

  property("splice double") = exists { (c: Double) =>
    q"$c" ≈ Literal(Constant(c))
  }

  property("splice boolean") = exists { (c: Boolean) =>
    q"$c" ≈ Literal(Constant(c))
  }

  property("splice string") = exists { (c: String) =>
    q"$c" ≈ Literal(Constant(c))
  }

  property("splice unit") = exists { (c: Unit) =>
    q"$c" ≈ Literal(Constant(c))
  }

  property("lift symbol") = exists { (u: Unit) =>
    val s = rootMirror.staticClass("scala.Int")
    q"$s" ≈ Ident(s)
  }

  property("lift type") = exists { (u: Unit) =>
    val tpe = rootMirror.staticClass("scala.Int").toType
    q"$tpe" ≈ TypeTree(tpe)
  }

  property("lift type tag") = exists { (u: Unit) =>
    val tag = TypeTag.Int
    q"$tag" ≈ TypeTree(tag.tpe)
  }

  property("lift weak type tag") = exists { (u: Unit) =>
    val tag = WeakTypeTag.Int
    q"$tag" ≈ TypeTree(tag.tpe)
  }
}