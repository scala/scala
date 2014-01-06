import org.scalacheck._
import scala.reflect.runtime.universe._

case class Simple(s: String, i: Int)
case class Nested(d: Double, s: String, simple: Simple)
case class `Simple.With.Dot`(d: Double)

case class Recursive(recursive: Recursive)

case class Mutually(rec: Rec)
case class Rec(mutually: Mutually)

package inside.a {
  case class Package(pkg: String)
}

object LiftableCaseClassProps extends QuasiquoteProperties("liftable case class") {
  property("lift simple case class") = test {
    val simple = Simple("simple", 42)
    assert(q"$simple" ≈ Apply(Ident(TermName("Simple")), List(Literal(Constant("simple")), Literal(Constant(42)))))
  }

  property("lift nested case class") = test {
    val simple = Simple("simple", 42)
    val nested = Nested(2.0, "nested", simple)
    assert(q"$nested" ≈ Apply(Ident(TermName("Nested")), List(Literal(Constant(2.0)), Literal(Constant("nested")), Apply(Ident(TermName("Simple")), List(Literal(Constant("simple")), Literal(Constant(42)))))))
  }

  property("lift case class with dots in name") = test {
    val dotted = `Simple.With.Dot`(3.0)
    assert(q"$dotted" ≈ Apply(Ident(TermName("Simple$u002EWith$u002EDot")), List(Literal(Constant(3.0)))))
  }

  property("lift case class inside a package") = test {
    val packaged = inside.a.Package("package")
    assert(q"$packaged" ≈ Apply(Ident(TermName("Package")), List(Literal(Constant("package")))))
  }

  property("materialize liftable instances for recursive and mutually recursive case classes") = test {
    implicitly[Liftable[Recursive]]
    implicitly[Liftable[Mutually]]
    implicitly[Liftable[Rec]]
    assert(true)
  }
}
