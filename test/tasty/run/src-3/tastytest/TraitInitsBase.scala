package tastytest

trait TraitInitsBase {
  val foo = 23
}
object TraitInitsBase {
  trait SubTrait extends TraitInitsBase
}
