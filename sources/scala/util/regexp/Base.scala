// $Id$

package scala.util.regexp ;

/** basic regular expressions */

trait Base {

  type regexp <: RegExp;

  abstract class RegExp {
    val isNullable:Boolean;
  }

  /** Alt( R,R,R* ) */
  case class  Alt(rs: regexp*)  extends RegExp {

    // check rs \in R,R,R*
    // @todo: flattening
    if({ val it = rs.elements; !it.hasNext || {it.next; !it.hasNext }})
      throw new SyntaxError("need at least 2 branches in Alt");

    final val isNullable = {
      val it = rs.elements;
      while( it.hasNext && it.next.isNullable ) {}
      !it.hasNext
    }
  }
  case class  Sequ(rs: regexp*) extends RegExp {
    // @todo: flattening
    // check rs \in R,R*
    if({ val it = rs.elements; !it.hasNext })
      throw new SyntaxError("need at least 1 item in Sequ");

    final val isNullable = {
      val it = rs.elements;
      while( it.hasNext && it.next.isNullable ) {}
      !it.hasNext
    }
  }

  case class  Star(r: regexp)   extends RegExp {
    final val isNullable = true;
  }

  case object Eps               extends RegExp {
    final val isNullable = true;
    override def toString() = "Eps";
  }

  /** this class can be used to add meta information to regexps */
  class Meta( r1:regexp )       extends RegExp {
    final val isNullable = r1.isNullable;
    def r = r1;
  }

  final def mkSequ(rs: regexp*): RegExp =
    if(!rs.elements.hasNext)
      Eps
    else
      Sequ(rs:_*);

}
