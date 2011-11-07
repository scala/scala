/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.dbc
package result


/** An ISO-9075:2003 (SQL) table. This is equivalent to a relation in the
 * relational model. */
@deprecated(DbcIsDeprecated, "2.9.0") abstract class Relation
extends collection.AbstractIterable[Tuple]
   with Iterable[Tuple] {

  /** The statement that generated this relation. */
  def statement: scala.dbc.statement.Relation

  /** A JDBC result containing this relation. */
  protected def sqlResult: java.sql.ResultSet

  /** A JDBC metadata object attached to the relation. */
  protected def sqlMetadata: java.sql.ResultSetMetaData = sqlResult.getMetaData()

  /** Metadata about all fields in a tuple of the relation. */
  def metadata: List[FieldMetadata] =
    for (count <- List.range(1, sqlMetadata.getColumnCount()+1)) yield
      new FieldMetadata {
        val name: String = sqlMetadata.getColumnName(count)
        val index: Int = count
        val datatype: DataType = dbc.datatype.Factory.create(sqlMetadata,count)
        val catalog: String = sqlMetadata.getCatalogName(count)
        val schema: String = sqlMetadata.getSchemaName(count)
        val table: String = sqlMetadata.getTableName(count)
      }

  /** Metadata about the field at the given index. If there is no such
   * field <code>None</code> is returned instead. */
  def metadataFor (index:Int): Option[FieldMetadata] = {
    val meta = metadata
    if (meta.length > index)
      Some(meta(index))
    else
      None
  }

  /** Metadata about the field with the given column name. If there is no
   * such field, <code>None</code> is returned instead. */
  def metadataFor (name:String): Option[FieldMetadata] =
    metadata.find(f=>(f.name==name));

  /** An iterator on the tuples of the relation.
   * <h3>Caution</h3> A Relation only has one single iterator, due to limitations
   * in DBMS. This means that if this method is called multiple times, all returned
   * iterators will share the same state. */
  def iterator: Iterator[Tuple] = new collection.AbstractIterator[Tuple] {
    protected val result: java.sql.ResultSet = Relation.this.sqlResult
    def hasNext: Boolean = resultNext
    private var resultNext = result.next()
    def next: Tuple = {
      if (resultNext) {
        val newTuple = new Tuple {
          val me = this
          val originatingRelation = Relation.this
          val fields: List[Field] = for (fieldMetadata <- metadata) yield
            new Field {
              val metadata = fieldMetadata
              val content = dbc.value.Factory.create(result,metadata.index,metadata.datatype)
              val originatingTuple = me
            }
        }
        resultNext = result.next()
        newTuple
      }
      else sys.error("next on empty iterator")
    }
  }

}
