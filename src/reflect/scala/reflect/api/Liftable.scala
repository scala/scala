package scala.reflect
package api

trait Liftable[T] {
  def apply(universe: api.Universe, value: T): universe.Tree
}

object Liftable {
  private class LiftableConstant[T] extends Liftable[T] {
    def apply(universe: Universe, value: T): universe.Tree =
      universe.Literal(universe.Constant(value))
  }

  implicit lazy val liftByte: Liftable[Byte] = new LiftableConstant[Byte]
  implicit lazy val liftShort: Liftable[Short] = new LiftableConstant[Short]
  implicit lazy val liftChar: Liftable[Char] = new LiftableConstant[Char]
  implicit lazy val liftInt: Liftable[Int] = new LiftableConstant[Int]
  implicit lazy val liftLong: Liftable[Long] = new LiftableConstant[Long]
  implicit lazy val liftFloat: Liftable[Float] = new LiftableConstant[Float]
  implicit lazy val liftDouble: Liftable[Double] = new LiftableConstant[Double]
  implicit lazy val liftBoolean: Liftable[Boolean] = new LiftableConstant[Boolean]
  implicit lazy val liftString: Liftable[String] = new LiftableConstant[String]
  implicit lazy val liftUnit: Liftable[Unit] = new LiftableConstant[Unit]

  implicit lazy val liftScalaSymbol: Liftable[scala.Symbol] = new Liftable[scala.Symbol] {
    def apply(universe: Universe, value: scala.Symbol): universe.Tree = {
      import universe._
      val symbol = Select(Ident(TermName("scala")), TermName("Symbol"))
      Apply(symbol, List(Literal(Constant(value.name))))
    }
  }
}
