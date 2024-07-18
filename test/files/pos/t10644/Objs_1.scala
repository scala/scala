//> using options -Xfatal-warnings
case object A ; case object B
object C {
// inferred refinement type `Product with Serializable` of val `objs` has owner `C`
// (and thus the receiver of the equality check was seen as effectivelyFinal,
// which then boosted our confidence in being able to say something about how
// final types compare for equality...)
  val objs = Seq(A, B)
}
