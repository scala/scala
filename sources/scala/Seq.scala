/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;

trait Seq[+A] with Function1[Int, A] with Iterable[A] {

    def length: Int;

    override def toString() = {
      def toString1( it:Iterator[ A ] ):String = {
	if( it.hasNext ) {
	  ",".concat( it.next.toString() )
             .concat( toString1( it ) )
	  } else ")";
      }
      val it = elements;
      if( it.hasNext )
	"Seq(" +  it.next.toString() + toString1( it ) ;
      else
	"Seq()"
      }
}
