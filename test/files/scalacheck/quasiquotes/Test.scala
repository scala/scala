import org.scalacheck._

object Test extends Properties("quasiquotes") {
  include(TermConstructionProps)
  include(TermDeconstructionProps)
  include(TypeConstructionProps)
  include(TypeDeconstructionProps)
  include(PatternConstructionProps)
  include(PatternDeconstructionProps)
  include(LiftableProps)
  include(ErrorProps)
  include(DefinitionConstructionProps)
  include(DefinitionDeconstructionProps)
}