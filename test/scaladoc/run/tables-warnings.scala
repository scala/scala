import scala.tools.nsc.doc.model._
import scala.tools.nsc.doc.base.comment._
import scala.tools.partest.ScaladocModelTest
import ColumnOption._

// Test with:
// partest --verbose --srcpath scaladoc test/scaladoc/run/tables-warnings.scala

object Test extends ScaladocModelTest {

  import access._

  override def resourceFile = "tables-warnings.scala"

  def scaladocSettings = ""

  def testModel(rootPackage: Package): Unit = {

    val base = rootPackage._package("scala")._package("test")._package("scaladoc")._package("tables")._package("warnings")

    val printCommentName = false

    def withComment(commentNames: String*)(test: Comment => Unit) = {
      commentNames foreach {
        commentName =>
          if (printCommentName) {
            println(commentName)
          }
          val comment = getComment(commentName, base)
          test(comment)
      }
    }

    /* Compact table creation */

    def pt(content: String): Paragraph = Paragraph(Text(content))

    def c(contents: String*): Cell = Cell(contents.toList.map(pt))

    def r(contents: String*): Row = Row(contents.toList.map(content => c(content)))

    withComment("PrematureEndOfText") { comment =>
      val header = r("Header")
      val colOpts = ColumnOptionLeft :: Nil
      val table = Table(header, colOpts, Nil)
      val summary = Paragraph(Chain(List(Summary(Text("|cell")))))
      val body = Body(table :: summary :: Nil)
      assertBodiesEquals(body, comment.body)
    }

    withComment("MissingTrailingCellMark") { comment =>
      val colOpts = ColumnOptionLeft :: Nil
      val table1 = Table(r("Unterminated"), colOpts, r("r1c1") :: Nil)
      //      val rows = r("r1c1") :: r("r2c1") :: r("r3c1") :: Nil
      val summary = Paragraph(Chain(List(Summary(Text("|r2c1")))))
      val table2 = Table(r("r3c1"), colOpts, Nil)
      val body = Body(table1 :: summary :: table2 :: Nil)
      assertBodiesEquals(body, comment.body)
    }

    withComment("InvalidColumnOptions") { comment =>
      val header = r("colon-colon", "middle-colon", "random", "center")
      val colOpts = ColumnOptionRight :: ColumnOptionRight :: ColumnOptionRight :: ColumnOptionCenter :: Nil
      val row = r("a", "b", "c", "d")
      val rows = row :: Nil
      assertTableEquals(Table(header, colOpts, rows), comment.body)
    }

    withComment("InvalidMarkdownUsingColumnOptions") { comment =>
      val header = r("Sequence")
      val colOpts = ColumnOptionRight :: Nil
      val row = r("9")
      val rows = row :: Nil
      assertTableEquals(Table(header, colOpts, rows), comment.body)
    }
  }

  private def getComment(traitName: String, containingPackage: Package): Comment = {
    containingPackage._trait(traitName).comment.get
  }

  private def assertTableEquals(expectedTable: Table, actualBody: Body): Unit = {
    actualBody.blocks.toList match {
      case (actualTable: Table) :: Nil =>
        assert(expectedTable == actualTable, s"\n\nExpected:\n${multilineFormat(expectedTable)}\n\nActual:\n${multilineFormat(actualTable)}\n")
      case _ =>
        val expectedBody = Body(List(expectedTable))
        assert(expectedBody == actualBody, s"Expected: $expectedBody, Actual: $actualBody")
    }
  }

  private def assertTableEquals(expectedTable: Table, actualBlock: Block): Unit = {
    assert(expectedTable == actualBlock, s"Expected: $expectedTable, Actual: $actualBlock")
  }

  private def assertBodiesEquals(expectedBody: Body, actualBody: Body): Unit = {
    val blocks = expectedBody.blocks zip actualBody.blocks
    val blockComparisons = blocks.zipWithIndex.collect {
      case ((expectedBlock, actualBlock), idx) if expectedBlock != actualBlock =>
        s"Block mismatch at index $idx\nExpected block: $expectedBlock\nActual block  : $actualBlock"
    }.headOption.getOrElse("")

    assert(expectedBody == actualBody, s"$blockComparisons\n\nExpected: $expectedBody, Actual: $actualBody")
  }

  private def multilineFormat(table: Table): String = {
    "header       : " + table.header + "\n" +
      "columnOptions: " + table.columnOptions.size + "\n" +
      (table.columnOptions mkString "\n") + "\n" +
      "rows         : " + table.rows.size + "\n" +
      (table.rows mkString "\n")
  }
}