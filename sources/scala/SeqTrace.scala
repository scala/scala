// needed for variable binding in pattern matching

package scala;

        // special sequence list

        // run of a left-transducer that tags its input with states

        // needed for variable binding during matching

        abstract class SeqTrace[ a ] extends List[ Tuple2[ Int, a ] ] {

             /*abstract*/ def isEmpty:boolean ;

             /*abstract*/ def head:Tuple2[ Int, a ];

             /*abstract*/ def headState:Int;

	     /*abstract*/ def headElem:a;


             /*abstract*/ def tail:SeqTrace[ a ];

        /*
             override def ::(zI:Int, za:a): SeqTrace[ a ] =
                   new SeqTraceCons (zI, za, this ) ;
*/
             // why the f&&k do I need the .as cast ?

             def add[ a ]( state:Int, value:a ):SeqTraceCons[ a ] =
                   SeqTraceCons[a](state, value, this as SeqTrace[ a ] );

      // this is copied verbatim from List... and SeqList
	 def mkString2(start: java.lang.String,
		     sep: java.lang.String,
		     end: java.lang.String): java.lang.String =
		       start +
		      (if (isEmpty) end
			 else if (tail.isEmpty) head.toString() + end
			       else head.toString().concat(sep).concat(
				             tail.mkString2("", sep, end)));
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


