package scala;

  /** nonempty SeqTrace
   */
  final case class SeqTraceCons[ b ]( hdI:Int, hdb:b, tl: SeqTrace[ b ] )
    extends SeqTrace[ b ]  {

	  def isEmpty = false;

	  def head = Tuple2( hdI, hdb );

	  def headState = hdI;

	  def headElem  = hdb;

	  def tail = tl;

	  override def toString(): String = mkString2("[ ", "; ", " ]");

    }


