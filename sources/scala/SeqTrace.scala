/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2003, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

// needed for variable binding in pattern matching

package scala;

// special sequence list

// run of a left-transducer that tags its input with states

// needed for variable binding during matching

abstract class SeqTrace[A] extends List[Pair[Int,A]] {

    /*abstract*/ def isEmpty: Boolean;

    /*abstract*/ def head: Pair[Int, A];

    /*abstract*/ def headState: Int;

    /*abstract*/ def headElem: A;

    /*abstract*/ def tail: SeqTrace[A];

/*
    override def ::(zI: Int, za: A): SeqTrace[A] =
       new SeqTraceCons(zI, za, this);
*/
    // why the f&&k do I need the .as cast ?

    def add[A](state: Int, value: A): SeqTraceCons[A] =
        SeqTraceCons[A](state, value, this as SeqTrace[A]);

    // this is copied verbatim from List... and SeqList
/*
    def mkString2(start: java.lang.String,
		  sep: java.lang.String,
		  end: java.lang.String): java.lang.String =
        start +
        (if (isEmpty) end
         else if (tail.isEmpty) head.toString() + end
         else head.toString().concat(sep).concat(
             tail.mkString2("", sep, end)));
*/
/* BUG
    override def mkString(start: java.lang.String,
		     sep: java.lang.String,
		     end: java.lang.String): java.lang.String =
        start +
        (if (isEmpty) end
         else if (tail.isEmpty) head.toString() + end
         else head.toString().concat(sep).concat(
             tail.mkString("", sep, end)));
*/

}
