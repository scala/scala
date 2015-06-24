trait Covariant[+A]
trait Invariant[A] extends Covariant[A @annotation.unchecked.uncheckedVariance] 
 
trait Combinable[G] {
  def combined = 0
}

trait CanBuildFrom[+C]
 
object C {
  implicit def convert1[G, TRAVONCE[+e] <: Covariant[e]]
    (xs: TRAVONCE[G]): Combinable[G] = ???
 
  implicit def convert2[G, SET[e] <: Invariant[e]]
    (xs: SET[_ <: G])
    (implicit cbf: CanBuildFrom[SET[G]]): Combinable[G] = ???

  implicit def cbf[A]: CanBuildFrom[Invariant[A]] = ???
}
// always failed
class Test1 {
  import C.{cbf, convert1, convert2}
  val s: Invariant[Nothing] = ???
  s.combined // fail
}
// didn't fail, now correctly fails
class Test2 {
  import C.{cbf, convert2, convert1}

  val s: Invariant[Nothing] = ???

  // Non-uniformity with Test1 was due to order of typechecking implicit candidates:
  // the last candidate typechecked was the only one that could contribute undetermined type parameters
  // to the enclosing context, due to mutation of `Context#undetparam` in `doTypedApply`.
  s.combined // was okay!
}


class TestExplicit {
  import C.{cbf, convert2}

  val s: Invariant[Nothing] = ???

  // Now the implicit Test fail uniformly as per this explicit conversion
  convert2(s).combined

  // Breaking this expression down doesn't make it work.
  {val c1 = convert2(s); c1.combined}
}

// These ones work before and after; inferring G=Null doesn't need to contribute an undetermined type param.
class Test3 {
   import C.{cbf, convert1, convert2}
   val s: Invariant[Null] = ???
   s.combined // okay
}

class Test4 {
   import C.{cbf, convert2, convert1}

   val s: Invariant[Null] = ???
   s.combined // okay
}
