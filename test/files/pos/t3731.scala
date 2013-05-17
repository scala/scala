object Test{
  trait ZW[S]{type T}
  def ZipWith[S, M <: ZW[S]]: M#T = sys.error("ZW")

  // meh must be parameterised to force an asSeenFrom that
  // duplicates the refinement in the TR's pre without updating its sym
  def meh[A] = ZipWith[A, ZW[A]{type T=Stream[A]}]

  meh[Int]: Stream[Int]
}
// debugging output in coevolveSym should say:
// coevolved type T#11029 : Stream#3234[A#9228] to type T#11277 : Stream#3234[A#9227]
// with Test.ZW#9219[A#9228]{type T#11029 = Stream#3234[A#9228]} -> Test.ZW#9219[A#9227]{type T#11277 = Stream#3234[A#9227]}
