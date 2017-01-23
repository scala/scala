import org.scalacheck._, Prop._, Gen._, Arbitrary._
import scala.reflect.runtime.universe._, Flag._

object UnliftableProps extends QuasiquoteProperties("unliftable") {
  property("unlift name") = test {
    val termname0 = TermName("foo")
    val typename0 = TypeName("foo")
    val q"${termname1: TermName}" = Ident(termname0)
    assert(termname1 == termname0)
    val q"${typename1: TypeName}" = Ident(typename0)
    assert(typename1 == typename0)
    val q"${name1: Name}" = Ident(termname0)
    assert(name1 == termname0)
    val q"${name2: Name}" = Ident(typename0)
    assert(name2 == typename0)
  }

  property("unlift type") = test {
    val q"${tpe: Type}" = TypeTree(typeOf[Int])
    assert(tpe =:= typeOf[Int])
  }

  property("unlift constant") = test {
    val q"${const: Constant}" = Literal(Constant("foo"))
    assert(const == Constant("foo"))
  }

  property("unlift char") = test {
    val q"${c: Char}" = Literal(Constant('0'))
    assert(c.isInstanceOf[Char] && c == '0')
  }

  property("unlift byte") = test {
    val q"${b: Byte}" = Literal(Constant(0: Byte))
    assert(b.isInstanceOf[Byte] && b == 0)
  }

  property("unlift short") = test {
    val q"${s: Short}" = Literal(Constant(0: Short))
    assert(s.isInstanceOf[Short] && s == 0)
  }

  property("unlift int") = test {
    val q"${i: Int}" = Literal(Constant(0: Int))
    assert(i.isInstanceOf[Int] && i == 0)
  }

  property("unlift long") = test {
    val q"${l: Long}" = Literal(Constant(0L: Long))
    assert(l.isInstanceOf[Long] && l == 0L)
  }

  property("unlift float") = test {
    val q"${f: Float}" = Literal(Constant(0.0f: Float))
    assert(f.isInstanceOf[Float] && f == 0.0f)
  }

  property("unlift double") = test {
    val q"${d: Double}" = Literal(Constant(0.0: Double))
    assert(d.isInstanceOf[Double] && d == 0.0)
  }

  property("unlift bool") = test {
    val q"${b: Boolean}" = q"true"
    assert(b.isInstanceOf[Boolean] && b == true)
  }

  property("unlift string") = test {
    val q"${s: String}" = q""" "foo" """
    assert(s.isInstanceOf[String] && s == "foo")
  }

  property("unlift scala.symbol") = test {
    val q"${s: scala.Symbol}" = q"'foo"
    assert(s.isInstanceOf[scala.Symbol] && s == 'foo)
  }

  implicit def unliftList[T: Unliftable]: Unliftable[List[T]] = Unliftable {
    case q"scala.collection.immutable.List(..$args)" if args.forall { implicitly[Unliftable[T]].unapply(_).nonEmpty } =>
      val ut = implicitly[Unliftable[T]]
      args.flatMap { ut.unapply(_) }
  }

  property("unlift list (1)") = test {
    val orig = List(1, 2)
    val q"${l1: List[Int]}" = q"$orig" // q"List(1, 2)"
    assert(l1 == orig)
    val q"f(..${l2: List[Int]})" = q"f(..$orig)" // q"f(1, 2)
    assert(l2 == orig)
  }

  property("unlift list (2)") = test {
    val orig2 = List(List(1, 2), List(3))
    val q"f(${l3: List[List[Int]]})" = q"f($orig2)" // q"f(List(List(1, 2), List(3)))
    assert(l3 == orig2)
    val q"f(..${l4: List[List[Int]]})" = q"f(..$orig2)" // q"f(List(1, 2), List(3))"
    assert(l4 == orig2)
    val q"f(...${l5: List[List[Int]]})" = q"f(...$orig2)" // q"f(1, 2)(3)
    assert(l5 == orig2)
  }

  property("don't unlift non-tree unquotee (1)") = test {
    val q"${a: TermName}.${b: TermName}" = q"a.b"
    assert(a == TermName("a"))
    assert(b == TermName("b"))
  }

  property("don't unlift non-tree unquotee (2)") = test {
    val q"${mods: Modifiers} def foo" = q"def foo"
    assert(mods == Modifiers(DEFERRED))
  }

  property("unlift tuple") = test {
    val q"${t2: (Int, Int)}" = q"(1, 2)"
    val q"${t3: (Int, Int, Int)}" = q"(1, 2, 3)"
    val q"${t4: (Int, Int, Int, Int)}" = q"(1, 2, 3, 4)"
    val q"${t5: (Int, Int, Int, Int, Int)}" = q"(1, 2, 3, 4, 5)"
    val q"${t6: (Int, Int, Int, Int, Int, Int)}" = q"(1, 2, 3, 4, 5, 6)"
    val q"${t7: (Int, Int, Int, Int, Int, Int, Int)}" = q"(1, 2, 3, 4, 5, 6, 7)"
    val q"${t8: (Int, Int, Int, Int, Int, Int, Int, Int)}" = q"(1, 2, 3, 4, 5, 6, 7, 8)"
    val q"${t9: (Int, Int, Int, Int, Int, Int, Int, Int, Int)}" = q"(1, 2, 3, 4, 5, 6, 7, 8, 9)"
    val q"${t10: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)}" = q"(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)"
    val q"${t11: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)}" = q"(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)"
    val q"${t12: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)}" = q"(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)"
    val q"${t13: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)}" = q"(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)"
    val q"${t14: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)}" = q"(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)"
    val q"${t15: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)}" = q"(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)"
    val q"${t16: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)}" = q"(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)"
    val q"${t17: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)}" = q"(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)"
    val q"${t18: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)}" = q"(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)"
    val q"${t19: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)}" = q"(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19)"
    val q"${t20: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)}" = q"(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)"
    val q"${t21: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)}" = q"(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)"
    val q"${t22: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)}" = q"(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)"
    // assert(t1 == Tuple1(1))
    assert(t2 == (1, 2))
    assert(t3 == (1, 2, 3))
    assert(t4 == (1, 2, 3, 4))
    assert(t5 == (1, 2, 3, 4, 5))
    assert(t6 == (1, 2, 3, 4, 5, 6))
    assert(t7 == (1, 2, 3, 4, 5, 6, 7))
    assert(t8 == (1, 2, 3, 4, 5, 6, 7, 8))
    assert(t9 == (1, 2, 3, 4, 5, 6, 7, 8, 9))
    assert(t10 == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
    assert(t11 == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
    assert(t12 == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
    assert(t13 == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
    assert(t14 == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
    assert(t15 == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
    assert(t16 == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
    assert(t17 == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17))
    assert(t18 == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18))
    assert(t19 == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19))
    assert(t20 == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20))
    assert(t21 == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21))
    assert(t22 == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22))
  }

  property("unlift xml comment") = test {
    implicit val unliftXmlComment = Unliftable[xml.Comment] {
      case q"new _root_.scala.xml.Comment(${value: String})" => xml.Comment(value)
    }
    val q"${comment: xml.Comment}" = q"<!--foo-->"
    assert(comment.commentText == "foo")
  }
}
