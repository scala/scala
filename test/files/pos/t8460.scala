object tan extends UFunc {
  implicit def ImplDouble: Impl[Double, Double] = ???
}

trait UFunc {
  trait TC[+A]
  type Impl[V, VR] = UFunc.UImpl[this.type, V, VR]
}

object UFunc {
  class UImpl[A, B, C]
  implicit def implicitDoubleUTag[Tag, V, VR](implicit conv: V=>Double, impl: UImpl[Tag, Double, VR]):UImpl[Tag, V, VR] = ???

}

object Test {
  implicitly[tan.Impl[Double, Double]]
  // we should discard the one and only divergent implicit (`implicitDoubleUTag`)
  // This is done under `scalac-hash v2.10.4 test.scala`, but not under
  // `scalac-hash v2.10.4 -Xdivergence211 test.scala`
  //
  // This seems to be because the companion implicits contain redundant entries
  //

}
