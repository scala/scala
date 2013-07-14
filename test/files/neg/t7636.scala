object Main extends App{
  class ResultTable[E]( query : Either[_,E] )( columns : Int )
  class C extends ResultTable(Left(5):Either[_,_])(5)
}
// Inference of the existential type for the parent type argument
// E still fails. That looks tricky to fix, see the comments in SI-7636.
// But we at least prevent a cascading NPE.