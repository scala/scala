package tastytest

import IntersectionErasure._

object TestIntersectionErasure extends Suite("TestIntersectionErasure") {

  def boxedId[T](t: T): T = t

  val bWithA: B with A = new B with A {} // dotc erases to A, scalac to B

  test("SAM bridges") {
    val sam: IntersectionSAM = x => x
    assert(sam(bWithA) === bWithA)
  }

  test("VC param")(
    assert(boxedId(new IntersectionVC(bWithA)).unwrapped == bWithA)
  )

  test("VC method unboxed")(
    assert(boxedId(new IntersectionVC(bWithA)).matchesInternal(bWithA))
  )

  test("VC method boxed")(
    assert(boxedId(new IntersectionVC(bWithA)).matches(new IntersectionVC(bWithA)))
  )

  test("VC parametric param")(
    assert(boxedId(new IntersectionVCParametric(bWithA)).unwrapped == bWithA)
  )

  test("VC parametric method unboxed")(
    assert(boxedId(new IntersectionVCParametric(bWithA)).matchesInternal(bWithA))
  )

  test("VC parametric method boxed")(
    assert(boxedId(new IntersectionVCParametric(bWithA)).matches(new IntersectionVCParametric(bWithA)))
  )

}
