package scala.runtime.matching ;

object Address {
  def empty = new Address();
}

/** Address holds the path in reverse Dewey notation
*/
class Address( l:Int* ) with Ordered[Address] {

  private val list:List[Int] = l.toList;

  def compareTo [b >: Address <% Ordered[b]](y: b): int = y match {
    case o:Address => List.view(list.reverse) compareTo o.list.reverse;
    case _ => -(y compareTo this)
  }

  def down: Address = new Address( ( 1 :: list ):_* );

  /** precond: p is nonempty */
  def right: Address = list.match {
    case List( i, rest@_* ) => new Address( ((i+1) :: rest ):_* )
  }

  override def toString() = list.mkString("Address(",".",")");

}
