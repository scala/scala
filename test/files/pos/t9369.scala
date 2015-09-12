object Test {

  trait Tree

  sealed abstract class Prop

  trait Simple extends Prop

  case class Atom(tree: Tree) extends Prop with Simple

  case class Not(prop: Prop) extends Prop with Simple

  def simplify1(prop: Prop): Prop = prop match {
    case Atom(tree) => ???
    case Not(prop)  => ???
    case _          => ???
  }

  def simplify2(prop: Prop): Prop = prop match {
    case Not(Atom(tree)) => ???
    case Not(Not(prop))  => ???
    case _               => ???
  }
}