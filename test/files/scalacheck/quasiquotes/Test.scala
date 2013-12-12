import org.scalacheck._

object Test extends Properties("quasiquotes") {
  include(TermConstructionProps)
  include(TermDeconstructionProps)
  include(TypeConstructionProps)
  include(TypeDeconstructionProps)
  include(PatternConstructionProps)
  include(PatternDeconstructionProps)
  include(LiftableProps)
  include(UnliftableProps)
  include(ErrorProps)
  include(RuntimeErrorProps)
  include(DefinitionConstructionProps)
  include(DefinitionDeconstructionProps)
  include(DeprecationProps)
  include(ForProps)
  include(TypecheckedProps)
}
