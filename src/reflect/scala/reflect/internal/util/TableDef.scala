package scala.reflect.internal.util

import TableDef._

/** A class for representing tabular data in a way that preserves
 *  its inner beauty.  See Exceptional for an example usage.
 *  One creates an instance of TableDef by defining the columns of
 *  the table, then uses that to create an instance of Table by
 *  passing in a sequence of rows.
 */
class TableDef[T](_cols: Column[T]*) {
  /** These operators are about all there is to it.
   *
   *  ~   appends a column to the table
   *  >>  creates a right-justified column and appends it
   *  <<  creates a left-justified column and appends it
   *  >+  specifies a string to separate the previous column from the next.
   *      if none is specified, a space is used.
   */
  def ~(next: Column[T])            = retThis(cols :+= next)
  def >>(pair: (String, T => Any))  = this ~ Column(pair._1, pair._2, false)
  def <<(pair: (String, T => Any))  = this ~ Column(pair._1, pair._2, true)
  def >+(sep: String)               = retThis(separators += ((cols.size - 1, sep)))

  /** Below this point should all be considered private/internal.
   */
  private var cols: List[Column[T]] = _cols.toList
  private var separators: Map[Int, String] = Map()

  def defaultSep(index: Int) = if (index > (cols.size - 2)) "" else " "
  def sepAfter(i: Int): String = separators.getOrElse(i, defaultSep(i))
  def sepWidths = cols.indices map (i => sepAfter(i).length)

  def columns = cols
  def colNames = cols map (_.name)
  def colFunctions = cols map (_.f)
  def colApply(el: T) = colFunctions map (f => f(el))
  def retThis(body: => Unit): this.type = { body ; this }

  class Table(val rows: Seq[T]) extends Seq[T] {
    def iterator          = rows.iterator
    def apply(index: Int) = rows(index)
    def length            = rows.length

    def maxColWidth(col: Column[T]) = col.name +: (rows map col.f) map (_.toString.length) max
    def specs = cols map (_ formatSpec rows)

    val colWidths   = cols map maxColWidth
    val rowFormat   = mkFormatString(sepAfter)
    val headFormat  = mkFormatString(i => " " * sepWidths(i))
    val argLists    = rows map colApply

    val headers = List(
      headFormat.format(colNames: _*),
      (colWidths, sepWidths).zipped map ((w1, w2) => "-" * w1 + " " * w2) mkString
    )

    def mkFormatString(sepf: Int => String): String =
      specs.zipWithIndex map { case (c, i) => c + sepf(i) } mkString

    def pp(): Unit = allToSeq foreach println

    def toFormattedSeq = argLists map (xs => rowFormat.format(xs: _*))
    def allToSeq = headers ++ toFormattedSeq

    override def toString = allToSeq mkString "\n"
  }

  def formatterFor(rows: Seq[T]): T => String = {
    val formatStr = new Table(rows).rowFormat

    x => formatStr.format(colApply(x) : _*)
  }

  def table(rows: Seq[T]) = new Table(rows)

  override def toString = cols.mkString("TableDef(", ", ", ")")
}

object TableDef {
  case class Column[-T](name: String, f: T => Any, left: Boolean) {
    def maxWidth(elems: Seq[T]): Int = name +: (elems map f) map (_.toString.length) max
    def formatSpec(elems: Seq[T]): String = {
      val justify = if (left) "-" else ""
      "%" + justify + maxWidth(elems) + "s"
    }
    override def toString = {
      val justify = if (left) "<<" else ">>"
      justify + "(" + name + ")"
    }
  }

  def apply[T](cols: Column[T]*) = new TableDef[T](cols: _*)
}
