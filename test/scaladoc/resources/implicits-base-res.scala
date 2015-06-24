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
 * def convToGtColonDoubleA(x: Double)    // enrichA3: with a constraint that T <: Double
 * def convToIntA(x: Int)                 // enrichA2: with a constraint that T = Int
 * def convToManifestA(x: T)              // enrichA7: with 2 constraints: T: Manifest and T <: Double
 * def convToMyNumericA(x: T)             // enrichA6: with a constraint that there is x: MyNumeric[T] implicit in scope
 * def convToNumericA(x: T)               // enrichA1: with a constraint that there is x: Numeric[T] implicit in scope
 * def convToEnrichedA(x: Bar[Foo[T]])    // enrichA5: no constraints, SHADOWED
 * def convToEnrichedA(x: S)              // enrichA4: with 3 constraints: T = Foo[Bar[S]], S: Foo and S: Bar, SHADOWED
 * def convToEnrichedA(x: T)              // enrichA0: with no constraints, SHADOWED
 * def convToTraversableOps(x: T)         // enrichA7: with 2 constraints: T: Manifest and T <: Double
 *                                        // should not be abstract!
 * }}}
 */
class A[T] {
  /** This should prevent the implicitly inherited `def convToEnrichedA: T` from `enrichA0` from showing up */
  def convToEnrichedA(x: T): T = sys.error("Let's check it out!")
  /** This should check implicit member elimination in the case of subtyping */
  def foo(a: T, b: AnyRef): T
}
/** Companion object with implicit transformations  */
object A {
  import language.implicitConversions // according to SIP18

  implicit def enrichA0[V](a: A[V]) = new EnrichedA(a)
  implicit def enrichA1[ZBUR: Numeric](a: A[ZBUR]) = new NumericA[ZBUR](a)
  implicit def enrichA2(a: A[Int]) = new IntA(a)
  implicit def enrichA3(a: A[T] forSome { type T <: Double }) = new GtColonDoubleA(a)
  implicit def enrichA4[S](a: A[Foo[Bar[S]]])(implicit foo: Foo[S], bar: Bar[S]): EnrichedA[S] = sys.error("not implemented")
  implicit def enrichA5[Z](a: A[Z]): EnrichedA[Bar[Foo[Z]]] = sys.error("not implemented")
  implicit def enrichA6[Z: MyNumeric](a: A[Z]) = new MyNumericA[Z](a)
  // TODO: Add H <: Double and see why it crashes for C and D -- context bounds, need to check!
  implicit def enrichA7[H <: Double : Manifest](a: A[H]) = new ManifestA[H](a) with MyTraversableOps[H] { def convToTraversableOps(x: H): H = sys.error("no") }
}


/** Class B
 *  - tests the existential type solving
 *  - the following inherited methods should appear:
 * {{{
 * def convToGtColonDoubleA(x: Double)      // enrichA3: no constraints
 * def convToManifestA(x: Double)           // enrichA7: no constraints
 * def convToMyNumericA(x: Double)          // enrichA6: (if showAll is set) with a constraint that there is x: MyNumeric[Double] implicit in scope
 * def convToNumericA(x: Double)            // enrichA1: no constraints
 * def convToEnrichedA(x: Bar[Foo[Double]]) // enrichA5: no constraints, SHADOWED
 * def convToEnrichedA(x: Double)           // enrichA0: no constraints, SHADOWED
 * def convToTraversableOps(x: Double)      // enrichA7: no constraints
 *                                          // should not be abstract!
 * }}}
 */
class B extends A[Double]
object B extends A


/** Class C
 *  - tests asSeenFrom
 *  - the following inherited methods should appear:
 * {{{
 * def convToIntA(x: Int)                 // enrichA2: no constraints
 * def convToMyNumericA(x: Int)           // enrichA6: (if showAll is set) with a constraint that there is x: MyNumeric[Int] implicit in scope
 * def convToNumericA(x: Int)             // enrichA1: no constraints
 * def convToEnrichedA(x: Int)            // enrichA0: no constraints, SHADOWED
 * def convToEnrichedA(x: Bar[Foo[Int]])  // enrichA5: no constraints, SHADOWED
 * }}}
 */
class C extends A[Int]
object C extends A


/** Class D
 *  - tests implicit elimination
 *  - the following inherited methods should appear:
 * {{{
 * def convToMyNumericA(x: String)        // enrichA6: (if showAll is set) with a constraint that there is x: MyNumeric[String] implicit in scope
 * def convToNumericA(x: String)          // enrichA1: (if showAll is set) with a constraint that there is x: Numeric[String] implicit in scope
 * def convToEnrichedA(x: Bar[Foo[String]]) // enrichA5: no constraints, SHADOWED
 * def convToEnrichedA(x: String)           // enrichA0: no constraints, SHADOWED
 * }}}
 */
class D extends A[String]
/** Companion object with implicit transformations  */
object D extends A


/** EnrichedA class <br/>
 *  - tests simple inheritance and asSeenFrom
 *  - A, B and C should be implicitly converted to this */
class EnrichedA[V](a: A[V]) {
  /** The convToEnrichedA: V documentation... */
  def convToEnrichedA(x: V): V = sys.error("Not implemented")
}

/** NumericA class <br/>
 *  - tests the implicit conversion between parametric and fixed types
 *  - A, B and C should be implicitly converted to this */
class NumericA[U: Numeric](a: A[U]) {
  /** The convToNumericA: U documentation... */
  def convToNumericA(x: U): U = implicitly[Numeric[U]].zero
}

/** IntA class <br/>
 *  - tests the interaction between implicit conversion and specific types
 *  - A and C should be implicitly converted to this */
class IntA(a: A[Int]) {
  /** The convToIntA: Int documentation... */
  def convToIntA(x: Int): Int = 0
}

/** GtColonDoubleA class <br/>
 *  - tests the interaction between implicit conversion and existential types
 *  - A and B should be implicitly converted to this */
class GtColonDoubleA(a: A[T] forSome { type T <: Double }) {
  /** The convToGtColonDoubleA: Double documentation... */
  def convToGtColonDoubleA(x: Double): Double = 0
}

/** MyNumericA class <br/>
 *  - tests the implicit conversion between parametric and fixed types
 *  - A should be implicitly converted to this */
class MyNumericA[U: MyNumeric](a: A[U]) {
  /** The convToMyNumericA: U documentation... */
  def convToMyNumericA(x: U): U = sys.error("dunno")
}

/** ManifestA class <br/>
 *  - tests the manifest recognition
 *  - A, B, C, D should be implicitly converted to this */
class ManifestA[W: Manifest](a: A[W]) {
  /** The convToManifestA: W documentation... */
  def convToManifestA(x: W): W = sys.error("dunno")
}

// [Eugene to Vlad] how do I test typetags here?

/** MyTraversableOps class <br/>
 *  - checks if any abstract members are added - should not happen!
 */
trait MyTraversableOps[S] {
  /** The convToTraversableOps: S documentation... */
  def convToTraversableOps(x: S): S
}
