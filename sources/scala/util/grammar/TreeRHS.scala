package scala.util.grammar;

/** right hand side of a tree production */
abstract class TreeRHS;

/** right hand side of a tree production, labelled with a letter from an alphabet */
case class  LabelledRHS[A](label: A, hnt: Int) extends TreeRHS;

case object AnyTreeRHS extends TreeRHS;
