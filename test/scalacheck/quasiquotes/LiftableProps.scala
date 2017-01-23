import org.scalacheck._, Prop._, Gen._, Arbitrary._
import scala.reflect.runtime.universe._, Flag._

object LiftableProps extends QuasiquoteProperties("liftable") {
  property("unquote byte") = test {
    val c: Byte = 0
    assert(q"$c" ≈ Literal(Constant(c)))
    assert(q"${0: Byte}" ≈ Literal(Constant(c)))
  }

  property("unquote short") = test {
    val c: Short = 0
    assert(q"$c" ≈ Literal(Constant(c)))
    assert(q"${0: Short}" ≈ Literal(Constant(c)))
  }

  property("unquote char") = test {
    val c: Char = 'c'
    assert(q"$c" ≈ Literal(Constant(c)))
    assert(q"${'c'}" ≈ Literal(Constant(c)))
  }

  property("unquote int") = test {
    val c: Int = 0
    assert(q"$c" ≈ Literal(Constant(c)))
    assert(q"${0: Int}" ≈ Literal(Constant(c)))
  }

  property("unquote long") = test {
    val c: Long = 0
    assert(q"$c" ≈ Literal(Constant(c)))
    assert(q"${0: Long}" ≈ Literal(Constant(c)))
  }

  property("unquote float") = test {
    val c: Float = 0.0f
    assert(q"$c" ≈ Literal(Constant(c)))
    assert(q"${0.0f: Float}" ≈ Literal(Constant(c)))
  }

  property("unquote double") = test {
    val c: Double = 0.0
    assert(q"$c" ≈ Literal(Constant(c)))
    assert(q"${0.0: Double}" ≈ Literal(Constant(c)))
  }

  property("unquote boolean") = test {
    val c: Boolean = false
    assert(q"$c" ≈ Literal(Constant(c)))
    assert(q"${true}" ≈ Literal(Constant(true)))
    assert(q"${false}" ≈ Literal(Constant(false)))
  }

  property("unquote string") = test {
    val c: String = "s"
    assert(q"$c" ≈ Literal(Constant(c)))
    assert(q"${"s"}" ≈ Literal(Constant(c)))
  }

  property("unquote unit") = test {
    val c: Unit = ()
    assert(q"$c" ≈ Literal(Constant(c)))
    assert(q"${()}" ≈ Literal(Constant(c)))
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

  val immutable = q"$scalapkg.collection.immutable"

  property("lift list variants") = test {
    val lst = List(1, 2)
    assert(q"$lst" ≈ q"$immutable.List(1, 2)")
    assert(q"f(..$lst)" ≈ q"f(1, 2)")
    val llst = List(List(1), List(2))
    assert(q"f(..$llst)" ≈ q"f($immutable.List(1), $immutable.List(2))")
    assert(q"f(...$llst)" ≈ q"f(1)(2)")
  }

  property("lift list of tree") = test {
    val lst = List(q"a", q"b")
    assert(q"$lst" ≈ q"$immutable.List(a, b)")
  }

  property("lift tuple") = test {
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

  property("lift nil") = test {
    val nil = Nil
    assert(q"$nil" ≈ q"scala.collection.immutable.Nil")
  }

  property("lift some") = test {
    val some1 = Some(1)
    assert(q"$some1" ≈ q"scala.Some(1)")
    val some2: Option[Int] = Some(1)
    assert(q"$some2" ≈ q"scala.Some(1)")
  }

  property("lift none") = test {
    val none1 = None
    assert(q"$none1" ≈ q"scala.None")
    val none2: Option[Int] = None
    assert(q"$none2" ≈ q"scala.None")
  }

  property("lift left") = test {
    val left1 = Left(1)
    assert(q"$left1" ≈ q"scala.util.Left(1)")
    val left2: Left[Int, Int] = Left(1)
    assert(q"$left2" ≈ q"scala.util.Left(1)")
    val left3: Either[Int, Int] = Left(1)
    assert(q"$left3" ≈ q"scala.util.Left(1)")
  }

  property("lift right") = test {
    val right1 = Right(1)
    assert(q"$right1" ≈ q"scala.util.Right(1)")
    val right2: Right[Int, Int] = Right(1)
    assert(q"$right2" ≈ q"scala.util.Right(1)")
    val right3: Either[Int, Int] = Right(1)
    assert(q"$right3" ≈ q"scala.util.Right(1)")
  }

  property("lift xml comment") = test {
    implicit val liftXmlComment = Liftable[xml.Comment] { comment =>
      q"new _root_.scala.xml.Comment(${comment.commentText})"
    }
    assert(q"${xml.Comment("foo")}" ≈ q"<!--foo-->")
  }
}
