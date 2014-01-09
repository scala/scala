class AbstractTable[T] { type TableElementType }
class Table[T] extends AbstractTable[T] { type TableElementType = T }
 
class Query[E, U]
class TableQuery[E <: AbstractTable[_]] extends Query[E, E#TableElementType]
 
object Test extends App {
  object MyTable extends TableQuery[Table[Long]]
 
  def list[R](q: Query[_, R]): List[R] = Nil
  list/*[Long]*/(MyTable) collect { case x => x }

  // Generates a redundant bridge method (double definition error)
  // in 2.10.x due to (at least) the bug in erasure fixed in SI-7120
}
