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

  property("unlift list variants (1)") = test {
    val orig = List(1, 2)
    val q"${l1: List[Int]}" = q"$orig" // q"List(1, 2)"
    assert(l1 == orig)
    val q"f(..${l2: List[Int]})" = q"f(..$orig)" // q"f(1, 2)
    assert(l2 == orig)
  }

  property("unlift list variants (2)") = test {
    val orig2 = List(List(1, 2), List(3))
    val q"f(${l3: List[List[Int]]})" = q"f($orig2)" // q"f(List(List(1, 2), List(3)))
    assert(l3 == orig2)
    val q"f(..${l4: List[List[Int]]})" = q"f(..$orig2)" // q"f(List(1, 2), List(3))"
    assert(l4 == orig2)
    val q"f(...${l5: List[List[Int]]})" = q"f(...$orig2)" // q"f(1, 2)(3)
    assert(l5 == orig2)
  }
}
