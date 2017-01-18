trait CoSet[U, +A <: U]
  extends CoSetLike[U, A, ({type S[A1 <: U] = CoSet[U, A1]})#S]

trait CoSetLike[U, +A <: U, +This[X] <: CoSetLike[U, A, This] with CoSet[U, A]] {

  implicitly[CoSet[U, Any]]
  // should report "implicit not found"
  // was triggering a StackOverflow as getClassParts looped over
  // the steam of types:
  // CoSet#6940[U#6966,A1#22868]
  // CoSet#6940[U#6966,A1#22876]
  // CoSet#6940[U#6966,A1#...]
}
