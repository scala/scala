// BE

package scala ;

    // SeqNil (empty SeqList)

    final case class SeqTraceNil[ c ]() extends SeqTrace[ c ]  {

	  def isEmpty = true;

	  def head: Tuple2[ Int, c ] = error("head of empty Trace");

	  def headState: Int = error("headState of empty Trace");

	  def headElem: c = error("headElem of empty Trace");

	  def tail: SeqTraceNil[c] = error("tail of empty Trace");

	  //override def toString(): String = "[]";

    }


