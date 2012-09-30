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
    implicit def convertManifest      [T: Manifest]      (a: A[T]) = new B(implicitly[Manifest[T]])
    implicit def convertClassManifest [T: ClassManifest] (a: A[T]) = new B(implicitly[ClassManifest[T]])
    implicit def convertOptManifest   [T: OptManifest]   (a: A[T]) = new B(implicitly[OptManifest[T]])
    implicit def convertClassTag      [T: ClassTag]      (a: A[T]) = new B(implicitly[ClassTag[T]])
    implicit def convertTypeTag       [T: TypeTag]       (a: A[T]) = new B(implicitly[TypeTag[T]])
    type K[X] = Numeric[X]
    type L[X] = Integral[X]
    type M[X] = Fractional[X]
    type N[X] = Manifest[X]
    type O[X] = ClassManifest[X]
    type P[X] = OptManifest[X]
    type Q[X] = ClassTag[X]
    type R[X] = TypeTag[X]
    implicit def convertK [T: K] (a: A[T]) = new B(implicitly[K[T]])
    implicit def convertL [T: L] (a: A[T]) = new B(implicitly[L[T]])
    implicit def convertM [T: M] (a: A[T]) = new B(implicitly[M[T]])
    implicit def convertN [T: N] (a: A[T]) = new B(implicitly[N[T]])
    implicit def convertO [T: O] (a: A[T]) = new B(implicitly[O[T]])
    implicit def convertP [T: P] (a: A[T]) = new B(implicitly[P[T]])
    implicit def convertQ [T: Q] (a: A[T]) = new B(implicitly[Q[T]])
    implicit def convertR [T: R] (a: A[T]) = new B(implicitly[R[T]])
  }
  class B[T](t: T) {
    def typeClass: T = t
  }
}