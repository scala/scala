// $Id$

package scala.util.regexp ;

/** basic regular expressions */

trait Base {

  type regexp <: RegExp;

  abstract class RegExp;

  /** Alt( R,R,R* ) */
  case class  Alt(rs: regexp*)  extends RegExp {

    // check rs \in R,R,R*

    if({ var i = 0;
        val it = rs.elements;
        while( it.hasNext ) {it.next; i=i+1};
        i} < 2)
      throw new SyntaxErrorExpression("need at least 2 branches in alt")
  }
  case class  Sequ(rs: regexp*) extends RegExp ;
  case class  Star(r: regexp)   extends RegExp ;
  case object Eps               extends RegExp {
    override def toString() = "Eps";
  }

  /** this class can be used to add meta information to regexps */
  class Meta( r:regexp )       extends RegExp ;

}
