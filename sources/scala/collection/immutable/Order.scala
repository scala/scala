/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.immutable;

/** The object Order provides constructors for orderings between
*   objects.
*/
object Order {
  /**
  *  Creates an Order given a predicate
  *  (<code>inferiority(e1,e2)</code>).
  *  The predicate is true iff e1 is less than
  *  e2. The ordinary equality operator == will be used for
  *  equality by the order.
  */
  def make[A](inferiority:(A,A) => Boolean) =
    new Order[A](inferiority,(e1:A,e2:A) => e1 == e2);
  /**
  *  Creates an Order given two predicates
  * (<code>inferiority(e1,e2)</code> and
  *  <code>equality(e1,e2)</code>). The first is true iff e1 is less than
  *  e2 and the second predicate is true when e1 is equal to e2.
  */
  def make[A](inferiority:(A,A) => Boolean, equality:(A,A) => Boolean) =
    new Order[A](inferiority,equality);
  /**
  *  Creates a 'standard' order between objects. <strong>NOTE:</strong>
  *  The order is arbitrary and uses the <code>hashCode()</code> of
  *  objects which might mean that the order is not really total
  *  for some objects <code>(!(e1 &lt; e2) && !(e2 &lt; e1) && !(e1==e2))</code>
  *  is true. This function should only be used when no other
  *  comparision predicates can be defined.
  */
  def make =
    new Order[Any](
      (e1:Any,e2:Any) =>
	e1.match {
	  case x:Int => {
	    e2.match {
	      case y:Int => x < y;
	      case y:Double => x < y;
	      case y:Float => x < y;
	      case y:Long => x < y;
	      case _ => true;
	    }
	  }
	  case x:Double => {
	    e2.match {
	      case y:Int => x < y;
	      case y:Double => x < y;
	      case y:Float => x < y;
	      case y:Long => x < y;
	      case _ => true;
	    }
	  }
	  case x:Float => {
	    e2.match {
	      case y:Int => x < y;
	      case y:Double => x < y
	      case y:Float => x < y;;
	      case y:Long => x < y;
	      case _ => true;
	    }
	  }
	  case x:Long => {
	    e2.match {
	      case y:Int => x < y;
	      case y:Double => x < y
	      case y:Float => x < y;;
	      case y:Long => x < y;
	      case _ => true;
	    }
	  }
	  case x:AnyRef => {
	    e2.match {
	      case y:AnyVal => false;
	      case y:AnyRef => x.hashCode() < y.hashCode();
	    }
	  }
	},
      (e1:Any,e2:Any) => e1 == e2);
}


/** The class Order[A] defines an order between objects of
 *  type A, given two predicates (<code>inferiority(e1,e2)</code> and
 *  <code>equality(e1,e2)</code>). The first is true iff e1 is less than
 *  e2 and the second predicate is true when e1 is equal to e2.
 *  An Order object can be used for ordered trees, maps, and sets.
 *  In order to use the order with the given
 *  TreeMap and TreeSet implementations the definition of the
 *  predicates <code>inferiority</code> and <code>equality</code>
 *  should be total. They predicate should
 *  also define a true total ordering (i.e., inferiority should be
 *  transitive, non-reflexive and asymmetric, and equality should be
 *  transitive, reflexive and symetric.)
 */
class Order[A](inferiority:(A,A) => Boolean, equality:(A,A) => Boolean) {
    def <  (e1:A, e2:A) = inferiority(e1,e2);
    def <= (e1:A, e2:A) = inferiority(e1,e2) || equality(e1,e2);
    def >  (e1:A, e2:A) = inferiority(e2,e1);
    def >= (e1:A, e2:A) = ! inferiority(e1,e2);
    def == (e1:A, e2:A) = equality(e1,e2);
    def != (e1:A, e2:A) = ! equality(e1,e2);
}
