trait Universe {
  type Tree

  type SymTree <: Tree
  type NameTree <: Tree
  type RefTree <: SymTree with NameTree

  type Ident <: RefTree
  type Select <: RefTree
}

object Test extends App {
  val universe: Universe = null
  import universe._
  def select: Select = ???
  def ident: Ident = ???
  List(select, ident)
}