import org.scalacheck._

object Test extends Properties("quasiquotes") {
  include(TermConstructionProps)
  include(TypeConstructionProps)
  include(TermDeconstructionProps)
  include(TypeDeconstructionProps)
  include(LiftableProps)
}