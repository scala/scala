/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;

/* use this for pattern matching on arrays. examples
<pre>
new IterableArray( args ).match {
      case Seq(x) => System.out.println("contains 1 arg");
      case Seq() => System.out.println("no arguments given");
};
</pre>
*/
class IterableArray[A]( arr:Array[A] ) with Seq[A]{
  def elements = new Iterator[A] {
    var i:int=0;
    def hasNext = {
      i < arr.length
    }
    def next:A = {
      val res = arr( i ); i=i+1; res
    }
  }
  def length = arr.length;
  def apply( i:int ) = arr( i );
}
