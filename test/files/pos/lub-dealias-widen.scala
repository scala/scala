import scala.language.higherKinds

sealed trait Path {
  type EncodeFunc
  type Route[R] = List[String] => R

  def >>(f: Route[Int]): Sitelet[EncodeFunc] = ???
}

case object PAny extends Path {
  type EncodeFunc = List[String] => String
}

case class PLit[Next <: Path]() extends Path {
  type EncodeFunc = Next#EncodeFunc
}

trait Sitelet[EncodeFunc] { self =>
  def &[G <: H, H >: EncodeFunc](that: Sitelet[G]): Sitelet[H] = ???
}

object Test {
  val r: Sitelet[Int => (Int => String)] = ???

  val p2: PLit[PAny.type] = ???
  val r2 /*: Sitelet[List[String] => String] */ // annotate type and it compiles with 2.10.0
     = p2 >> { (xs: List[String]) => 0 }

  // This works after https://github.com/scala/scala/commit/a06d31f6a
  // Before: error: inferred type arguments [List[String] => String,List[String] => String]
  //         do not conform to method &'s type parameter bounds
  //         [G <: H,H >: Int => (Int => String)]
  val s = r & r2
}