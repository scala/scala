/** Tests the "known type classes" feature of scaladoc implicits
 *  if the test fails, please update the correct qualified name of
 *  the type class in src/compiler/scala/tools/nsc/doc/Settings.scala
 *  in the knownTypeClasses map. Thank you! */
package scala.test.scaladoc.implicits.typeclasses {

  class A[T]
  object A {
    import language.implicitConversions
    import scala.reflect.ClassTag
    import scala.reflect.runtime.universe.TypeTag
    implicit def convertNumeric       [T: Numeric]       (a: A[T]) = new B(implicitly[Numeric[T]])
    implicit def convertIntegral      [T: Integral]      (a: A[T]) = new B(implicitly[Integral[T]])
    implicit def convertFractional    [T: Fractional]    (a: A[T]) = new B(implicitly[Fractional[T]])
    implicit def convertClassTag      [T: ClassTag]      (a: A[T]) = new B(implicitly[ClassTag[T]])
    implicit def convertTypeTag       [T: TypeTag]       (a: A[T]) = new B(implicitly[TypeTag[T]])
    type K[X] = Numeric[X]
    type L[X] = Integral[X]
    type M[X] = Fractional[X]
    type Q[X] = ClassTag[X]
    type R[X] = TypeTag[X]
    implicit def convertK [T: K] (a: A[T]) = new B(implicitly[K[T]])
    implicit def convertL [T: L] (a: A[T]) = new B(implicitly[L[T]])
    implicit def convertM [T: M] (a: A[T]) = new B(implicitly[M[T]])
    implicit def convertQ [T: Q] (a: A[T]) = new B(implicitly[Q[T]])
    implicit def convertR [T: R] (a: A[T]) = new B(implicitly[R[T]])
  }
  class B[T](t: T) {
    def typeClass: T = t
  }
}
