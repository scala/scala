/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2004, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.mutable;

/** This class allows appending of elements and sequences in O(1), in contrast
 *  to buffer, which adds sequences in O(k) where k is the length of the sequence.
 *  However, random access costs O(n), so this data structure should only be used
 *  if no modifications and no access is required, i.e. construction of sequences
 *  out of subsequences.
 *
 *  @author  Burak Emir
 */
final class AppendBuffer[ A ] with Seq[ A ] {

  private var len = 0;

  class MyElemList extends MutableList[ Option[A] ]  {
    def append( e: Option[A] ) = appendElem( e );
  }
  private val elemList = new MyElemList();

  class MySeqList extends MutableList[ Seq[A] ]  {
    def append( seq:Seq[A] ) =  appendElem( seq );
  }
  private val seqList = new MySeqList();

  def append( as:Seq[A] ) = {
    if( as.length > 0 ) {
      elemList.append( None );
      seqList.append( as );
      this.len = this.len + as.length;
    }
  }

  def append( a:A ) = {
    elemList.append( Some( a ) );
    this.len = this.len + 1;
  }

  def length = this.len;

  def apply( i:int ) = {
    val el = this.elements;
    var j = 0;
    while( j < i ) {
      j = j + 1;
      el.next;
    }
    el.next
  }

  def elements = new Iterator[A] {
    val itEl  = AppendBuffer.this.elemList.elements;
    val itSeq = AppendBuffer.this.seqList.elements;
    var curIt:Option[Iterator[A]] = None;
    def hasNext = itEl.hasNext || itSeq.hasNext;
    def next:A = curIt match {
      case None => itEl.next match {
        case Some( x ) => x
        case None => curIt = Some(itSeq.next.elements); next
      }
      case Some( z ) =>
        if( z.hasNext ) {
          z.next
        } else {
          curIt = None;
          next
        }
    }
  }
}
