package foo.bar.baz // the package nesting level material to this bug
 
class DivergenceTest {
 
  trait ColumnBase[T]
 
  trait ShapeLevel
  trait Flat extends ShapeLevel
  trait Lower extends Flat
 
  class Shape2[Level <: ShapeLevel, -M, U]
 
  implicit final def columnBaseShape[Level >: Flat <: ShapeLevel, T, C <: ColumnBase[_]]
                                    (implicit ev: C <:< ColumnBase[T]
                                    ): Shape2[Level, C, T] = ???

  implicit final def intShape[Level <: ShapeLevel, T]: Shape2[Level, Int, Int] = ???
  implicit final def tuple2Shape[Level <: ShapeLevel, M1,M2, U1,U2]
                                (implicit u1: Shape2[_ <: Level, M1, U1],
                                          u2: Shape2[_ <: Level, M2, U2]
                                ): Shape2[Level, (M1,M2), (U1,U2)] = ???
 
  def foo {
    class Coffees extends ColumnBase[Int]
 
    def map1[F, T](f: F)(implicit shape: Shape2[_ <: Flat, F, T]) = ???
 
    map1(((1, null: Coffees), 1))
    map1(((null: Coffees, 1), 1)) // fails with implicit divergence error in 2.11.0-M6, works under 2.10.3
  }
}
