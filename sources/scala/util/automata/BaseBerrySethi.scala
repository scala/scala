package scala.util.automata;

import scala.util.regexp.Base;

import scala.collection.mutable;
import scala.collection.immutable;

/** this turns a regexp over A into a NondetWorkAutom over A using the
 *  celebrated position automata construction (also called Berry-Sethi or
 *  Glushkov)
 */
abstract class BaseBerrySethi {

  val lang: Base;
  import lang.{Alt,Eps,Meta,RegExp,Sequ,Star} ;

  protected var pos = 0;;

  // maps a letter to an Integer ( the position )
  // is not *really* needed (preorder determines position!)
  protected var posMap: mutable.HashMap[RegExp, Int] = _;

  protected var globalFirst: immutable.Set[Int] = _;

  // results which hold all info for the NondetWordAutomaton
  protected var follow: mutable.HashMap[Int, immutable.Set[Int]] = _;

  protected var finalTag: Int = _;

  protected var finals: immutable.TreeMap[int,int] = _;      // final states

  // constants --------------------------

  final val emptySet:immutable.Set[Int] = new immutable.TreeSet[Int]();

  /** computes first( r ) for the word regexp r */
  protected def compFirst(r: RegExp): immutable.Set[Int] = r match {
    case x:Alt    =>
      var tmp = emptySet;
      val it = x.rs.elements;                       // union
        while( it.hasNext ) { tmp = tmp incl compFirst( it.next ); };
      tmp
    case Eps => emptySet;
    //case x:Letter => emptySet + posMap(x);  // singleton set
    case x:Meta => compFirst( x.r )
    case x:Sequ   =>
      var tmp = emptySet;
      val it = x.rs.elements;                       // union
      while( it.hasNext ) {
      val z = it.next;
      tmp = tmp incl compFirst( z );
      if( !z.isNullable )
        return tmp
      };
    tmp
    case _ => error("unexpected pattern "+r.getClass());
  }

  /** computes last( r ) for the regexp r */
  protected def compLast(r: RegExp): immutable.Set[Int] = r match {
    case x:Alt    =>
      var tmp = emptySet;
      val it = x.rs.elements;                       // union
      while( it.hasNext ) { tmp = tmp incl compFirst( it.next ); };
      tmp
    case Eps      => emptySet;
    //case x:Letter => emptySet + posMap(x) // singleton set
    case x:Meta   => compLast( x.r )
    case x:Sequ   =>
      var tmp = emptySet;
      val it = x.rs.elements.toList.reverse.elements;       // union
      while( it.hasNext ) {
        val z = it.next;
        tmp = tmp incl compLast( z );
        if( !z.isNullable )
          return tmp
      };
    tmp
    case Star(t)  => compLast(t);
    case _        => error("unexpected pattern " + r.getClass());
  }

  // starts from the right-to-left
  // precondition: pos is final
  //               pats are successor patterns of a Sequence node
  protected def compFollow(r: Seq[RegExp]): immutable.Set[Int] = {
    var first = emptySet;
    var fol = emptySet;
    if( r.length > 0 ) {//non-empty expr

      val it = r.elements.toList.reverse.elements;

      fol = fol + pos; // don't modify pos !
      while( it.hasNext ) {
        val p = it.next;
        first = compFollow1( fol, p );
        fol = if( p.isNullable )
          fol incl first
        else
          first;
      }
    }
    this.follow.update( 0, first );
    return first;
  }

  /** returns the first set of an expression, setting the follow set along
   *  the way
   */
  protected def compFollow1( fol1:immutable.Set[Int], r:RegExp): immutable.Set[Int] = {
    var fol = fol1;
    //System.out.println("compFollow1("+fol+","+pat+")");
    r match {

      case x:Alt =>
        var first = emptySet;
        val it = x.rs.elements.toList.reverse.elements;
        while( it.hasNext )
          first = first incl compFollow1( fol, it.next );
        first;

      /*
      case x:Letter =>
        val i = posMap( x );
        this.follow.update( i, fol );
        emptySet + i;
      */
      case x:Meta =>
        compFollow1( fol1, x.r );

      case x:Star =>
        fol = fol incl compFirst( x.r );
        compFollow1( fol, x.r );

      case x:Sequ =>
        var first = emptySet;
        val it = x.rs.elements.toList.reverse.elements;
        while( it.hasNext ) {
          val p = it.next;
          first = compFollow1( fol, p );
          fol = if( p.isNullable )
                  fol incl first ;
                else
                  first;
        }
        first;

      case _ => error("unexpected pattern: "+r.getClass());
    }
  }

  /** returns "Sethi-length" of a pattern, creating the set of position
   *  along the way
   */

  // todo: replace global variable pos with acc
  protected def traverse(r: RegExp): Unit = {
    r match {     // (is tree automaton stuff, more than Berry-Sethi)

      case x:Alt =>
        val it = x.rs.elements;
        while( it.hasNext ) traverse( it.next );

      case x:Sequ =>
        val it = x.rs.elements;
        while( it.hasNext ) traverse( it.next );

      case x:Meta           => traverse( x.r )

      case Star(t) =>
        traverse(t)

      case _ => error("unexp pattern "+ r.getClass());
    }
  }

}
