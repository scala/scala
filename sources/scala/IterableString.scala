/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;

/* use this to for pattern matching on strings. example
<pre>
 val s = "supercalifragilistic-expialidocious";
    new IterableString( s ).match {
      case Seq(_ *, 'f','r','a', _ *) => System.out.println("fra");
      case _ => System.out.println("this never happens");
  }
</pre>
*/
class IterableString( s:String ) with Seq[char]{
  def elements= new Iterator[char] {
    var i:int=0;
    def hasNext = {
      i < s.length()
    }
    def next:char = {
      val res = s.charAt( i ); i=i+1; res
    }
  }
  def length = s.length();
  def apply( i:int ) = s.charAt( i );
}
