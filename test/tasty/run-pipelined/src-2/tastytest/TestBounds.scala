package tastytest

import lib.Bounds
import lib.Bounds.{A, C, I, G, F, FExtra}

object TestBounds extends scala.App {
  class A1 extends C with I
  class G1 extends F[G1] with FExtra[G1]

  val a: A = new A
  val a1: A1 = new A1
  val g: G = new G
  val g1: G1 = new G1

  Bounds.m(a) // Testing A <: C with I when reading from Java TASTy.
  Bounds.m_ARRAY(Array(a)) // Testing A <: C with I when reading from Java TASTy.
  Bounds.m_WILDCARD_SUB(a.getClass) // Testing A <: C when reading from Java TASTy.
  Bounds.m_WILDCARD_SUP(classOf[C]) // Testing C >: A when reading from Java TASTy.

  Bounds.m(a1) // Testing A1 <: C with I when reading from Java TASTy.
  Bounds.m_ARRAY(Array(a1)) // Testing A1 <: C with I when reading from Java TASTy.
  Bounds.m_WILDCARD_SUB(a1.getClass) // Testing A1 <: C when reading from Java TASTy.

  Bounds.f(g) // Testing G <: F[G] with FExtra[G] when reading from Java TASTy.
  Bounds.f_ARRAY(Array(g)) // Testing G <: F[G] with FExtra[G] when reading from Java TASTy.
  Bounds.f_WILDCARD_SUB(g.getClass) // Testing G <: F[_] when reading from Java TASTy.
  Bounds.f_WILDCARD_SUP(classOf[F[_]]) // Testing F[_] >: G when reading from Java TASTy.

  Bounds.f(g1) // Testing G1 <: F[G] with FExtra[G] when reading from Java TASTy.
  Bounds.f_ARRAY(Array(g1)) // Testing G1 <: F[G] with FExtra[G] when reading from Java TASTy.
  Bounds.f_WILDCARD_SUB(g1.getClass) // Testing G1 <: F[_] when reading from Java TASTy.
}
