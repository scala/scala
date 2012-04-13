/**
 *  Test scaladoc implicits - the bread and butter of the testsuite :)
 */
package scala.test.scaladoc.implicits.base

class Foo[T]
class Bar[T]
trait MyNumeric[R]

/** Class A
 *  - tests the complete type inference
 *  - the following inherited methods should appear:
 * {{{
 * def convToGtColonDoubleA: Double    // pimpA3: with a constraint that T <: Double
 * def convToIntA: Int                 // pimpA2: with a constraint that T = Int
 * def convToManifestA: T              // pimpA7: with 2 constraints: T: Manifest and T <: Double
 * def convToMyNumericA: T             // pimpA6: with a constraint that there is x: MyNumeric[T] implicit in scope
 * def convToNumericA: T               // pimpA1: with a constraint that there is x: Numeric[T] implicit in scope
 * def convToPimpedA: Bar[Foo[T]]      // pimpA5: no constraints
 * def convToPimpedA: S                // pimpA4: with 3 constraints: T = Foo[Bar[S]], S: Foo and S: Bar
 * def convToTraversableOps: T         // pimpA7: with 2 constraints: T: Manifest and T <: Double
 *                                     // should not be abstract!
 * }}}
 */
class A[T] {
  /** This should prevent the implicitly inherited `def convToPimpedA: T` from `pimpA0` from showing up */
  def convToPimpedA: T = sys.error("Let's check it out!")
}
/** Companion object with implicit transformations  */
object A {
  implicit def pimpA0[V](a: A[V]) = new PimpedA(a)
  implicit def pimpA1[ZBUR: Numeric](a: A[ZBUR]) = new NumericA[ZBUR](a)
  implicit def pimpA2(a: A[Int]) = new IntA(a)
  implicit def pimpA3(a: A[T] forSome { type T <: Double }) = new GtColonDoubleA(a)
  implicit def pimpA4[S](a: A[Foo[Bar[S]]])(implicit foo: Foo[S], bar: Bar[S]): PimpedA[S] = sys.error("not implemented")
  implicit def pimpA5[Z](a: A[Z]): PimpedA[Bar[Foo[Z]]] = sys.error("not implemented")
  implicit def pimpA6[Z: MyNumeric](a: A[Z]) = new MyNumericA[Z](a)
  // TODO: Add H <: Double and see why it crashes for C and D -- context bounds, need to check!
  implicit def pimpA7[H <: Double : Manifest](a: A[H]) = new ManifestA[H](a) with MyTraversableOps[H] { def convToTraversableOps: H = sys.error("no") }
}


/** Class B
 *  - tests the existential type solving
 *  - the following inherited methods should appear:
 * {{{
 * def convToGtColonDoubleA: Double    // pimpA3: no constraints
 * def convToManifestA: Double         // pimpA7: no constraints
 * def convToMyNumericA: Double        // pimpA6: (if showAll is set) with a constraint that there is x: MyNumeric[Double] implicit in scope
 * def convToNumericA: Double          // pimpA1: no constraintsd
 * def convToPimpedA: Bar[Foo[Double]] // pimpA5: no constraints
 * def convToTraversableOps: Double    // pimpA7: no constraints
 *                                     // should not be abstract!
 * }}}
 */
class B extends A[Double]
object B extends A


/** Class C
 *  - tests asSeenFrom
 *  - the following inherited methods should appear:
 * {{{
 * def convToIntA: Int                 // pimpA2: no constraints
 * def convToMyNumericA: Int           // pimpA6: (if showAll is set) with a constraint that there is x: MyNumeric[Int] implicit in scope
 * def convToNumericA: Int             // pimpA1: no constraints
 * def convToPimpedA: Bar[Foo[Int]]    // pimpA5: no constraints
 * }}}
 */
class C extends A[Int]
object C extends A


/** Class D
 *  - tests implicit elimination
 *  - the following inherited methods should appear:
 * {{{
 * def convToMyNumericA: String        // pimpA6: (if showAll is set) with a constraint that there is x: MyNumeric[String] implicit in scope
 * def convToNumericA: String          // pimpA1: (if showAll is set) with a constraint that there is x: Numeric[String] implicit in scope
 * def convToPimpedA: Bar[Foo[String]] // pimpA5: no constraints
 * }}}
 */
class D extends A[String]
/** Companion object with implicit transformations  */
object D extends A


/** PimpedA class <br/>
 *  - tests simple inheritance and asSeenFrom
 *  - A, B and C should be implicitly converted to this */
class PimpedA[V](a: A[V]) {
  /** The convToPimpedA: V documentation... */
  def convToPimpedA: V = sys.error("Not implemented")
}

/** NumericA class <br/>
 *  - tests the implicit conversion between parametric and fixed types
 *  - A, B and C should be implicitly converted to this */
class NumericA[U: Numeric](a: A[U]) {
  /** The convToNumericA: U documentation... */
  def convToNumericA: U = implicitly[Numeric[U]].zero
}

/** IntA class <br/>
 *  - tests the interaction between implicit conversion and specific types
 *  - A and C should be implicitly converted to this */
class IntA(a: A[Int]) {
  /** The convToIntA: Int documentation... */
  def convToIntA: Int = 0
}

/** GtColonDoubleA class <br/>
 *  - tests the interaction between implicit conversion and existential types
 *  - A and B should be implicitly converted to this */
class GtColonDoubleA(a: A[T] forSome { type T <: Double }) {
  /** The convToGtColonDoubleA: Double documentation... */
  def convToGtColonDoubleA: Double = 0
}

/** MyNumericA class <br/>
 *  - tests the implicit conversion between parametric and fixed types
 *  - A should be implicitly converted to this */
class MyNumericA[U: MyNumeric](a: A[U]) {
  /** The convToMyNumericA: U documentation... */
  def convToMyNumericA: U = sys.error("dunno")
}

/** ManifestA class <br/>
 *  - tests the manifest recognition
 *  - A, B, C, D should be implicitly converted to this */
class ManifestA[W: Manifest](a: A[W]) {
  /** The convToManifestA: W documentation... */
  def convToManifestA: W = sys.error("dunno")
}

/** MyTraversableOps class <br/>
 *  - checks if any abstract members are added - should not happen!
 */
trait MyTraversableOps[S] {
  /** The convToTraversableOps: S documentation... */
  def convToTraversableOps: S
}

