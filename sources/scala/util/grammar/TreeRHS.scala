package scala.util.grammar;

import scala.util.alphabet.Alphabet ;

/** right hand side of a tree production */
abstract class TreeRHS;

/** right hand side of a tree production, labelled with a letter from an alphabet */
case class  LabelledTreeRHS[A <: Alphabet](label: A, hnt: Int) extends TreeRHS;

case object AnyTreeRHS extends TreeRHS;
