package scala.util.grammar;

abstract class HedgeRHS;

/** right hand side of a hedge production, deriving a single tree */
case class  ConsRHS(tnt: Int, hnt: Int) extends HedgeRHS;

/** right hand side of a hedge production, deriving any hedge */
case object AnyHedgeRHS extends HedgeRHS;

/** right hand side of a hedge production, deriving the empty hedge */
case object EmptyHedgeRHS extends HedgeRHS;
