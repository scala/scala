import scala.reflect.macros.whitebox

trait Derivation[A]

object Derivation {
  case class Successful[A]() extends Derivation[A]
  case class Failed[A](failures: List[String]) extends Derivation[A]

  var failures = List.empty[String]

  def materializeDerivationImpl[A](c: whitebox.Context)(implicit tt: c.WeakTypeTag[A]): c.Tree = {
    import c.universe._

    c.inferImplicitValue(weakTypeOf[A]) match {
      case EmptyTree if c.openImplicits.length == 1 =>
        q"Derivation.Failed[${weakTypeOf[A]}](Nil)"

      case EmptyTree =>
        failures ::= weakTypeOf[A].toString
        q"Derivation.Failed[${weakTypeOf[A]}](Nil)"

      case _ if c.openImplicits.length == 1 && failures.nonEmpty =>
        val tree = q"Derivation.Failed[${weakTypeOf[A]}](List(..$failures))"
        failures = Nil
        tree

      case _ =>
        q"Derivation.Successful[${weakTypeOf[A]}]()"
    }
  }

  implicit def materializeDerivation[A]: Derivation[A] = macro materializeDerivationImpl[A]
}
