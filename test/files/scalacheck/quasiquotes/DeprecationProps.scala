import org.scalacheck._, Prop._, Gen._, Arbitrary._
import scala.reflect.runtime.universe._

object DeprecationProps extends QuasiquoteProperties("deprecation") {
  val tname = TypeName("Foo")
  val tpt = tq"Foo"
  val tpe = typeOf[Int]
  val sym = tpe.typeSymbol.asType
  val argss = List(q"x") :: List(q"y") :: Nil
  val args = q"x" :: q"y" :: Nil

  property("new tpt argss") = test {
    assert(q"new $tpt(...$argss)" ≈ New(tpt, argss))
  }

  property("new tpe args") = test {
    assert(q"new $tpe(..$args)" ≈ New(tpe, args: _*))
  }

  property("new tpe args") = test {
    assert(q"new ${sym.toType}(..$args)" ≈ New(sym, args: _*))
  }

  property("apply sym args") = test {
    assert(q"$sym(..$args)" ≈ Apply(sym, args: _*))
  }

  property("applyconstructor") = test {
    assert(q"new $tpt(..$args)" ≈ ApplyConstructor(tpt, args))
  }

  property("super sym name") = test {
    assert(q"$sym.super[$tname].x".qualifier ≈ Super(sym, tname))
  }

  property("throw tpe args") = test {
    assert(q"throw new $tpe(..$args)" ≈ Throw(tpe, args: _*))
  }

  property("casedef pat body") = test {
    val pat = pq"foo"
    val body = q"bar"
    assert(cq"$pat => $body" ≈ CaseDef(pat, body))
  }

  property("try body cases") = test {
    val cases = (pq"a", q"b") :: (pq"c", q"d") :: Nil
    val newcases = cases.map { case (pat, body) => cq"$pat => $body" }
    val body = q"foo"
    assert(q"try $body catch { case ..$newcases }" ≈ Try(body, cases: _*))
  }
}