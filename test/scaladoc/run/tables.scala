import scala.tools.nsc.doc.model._
import scala.tools.nsc.doc.base.comment._
import scala.tools.partest.ScaladocModelTest
import ColumnOption._

// Test with:
// partest --verbose --srcpath scaladoc test/scaladoc/run/tables.scala

object Test extends ScaladocModelTest {

  import access._

  override def resourceFile = "tables.scala"

  def scaladocSettings = ""

  def testModel(rootPackage: Package): Unit = {

    val base = rootPackage._package("scala")._package("test")._package("scaladoc")._package("tables")

    val allTests = true
    val whitelist = Set[String]()
    val blacklist = Set[String]()
    val whitelistPrefix: Option[String] = None
    val printCommentName = false

    def includeTest(commentName: String) = {
      val whitelisted = whitelist(commentName) || whitelistPrefix.map(commentName startsWith _).getOrElse(false)
      (allTests && !blacklist(commentName)) || whitelisted
    }

    def withComment(commentNames: String*)(test: Comment => Unit) = {
      commentNames foreach {
        commentName =>
          if (includeTest(commentName)) {
            if (printCommentName) {
              println(commentName)
            }
            val comment = getComment(commentName, base)
            test(comment)
          }
      }
    }

    /* Compact table creation */

    def pt(content: String): Paragraph = Paragraph(Text(content))

    def c(contents: String*): Cell = Cell(contents.toList.map(pt))

    def ci(content: Inline): Cell = Cell(Paragraph(content) :: Nil)

    /* None transforms to an empty block list */
    def r(contents: Any*): Row = {
      val cells = contents.toList.map {
        case "" => Cell(Nil)
        case x: String => c(x)
        case None => Cell(Nil)
      }
      Row(cells)
    }

    withComment("Minimal") { comment =>
      val header = r("First Header")
      val colOpts = ColumnOptionLeft :: Nil
      val row = r("Content Cell")
      assertTableEquals(Table(header, colOpts, row :: Nil), comment.body)
    }

    withComment("NoDataRows") { comment =>
      val header = r("No Data Rows")
      val colOpts = ColumnOptionLeft :: Nil
      assertTableEquals(Table(header, colOpts, Nil), comment.body)
    }

    withComment("ColumnOptionsAllTypes", "ColumnOptionsMoreThanThreeHyphens") { comment =>
      val header = r("First Header", "Second Header", "Third Header")
      val colOpts = ColumnOptionLeft :: ColumnOptionCenter :: ColumnOptionRight :: Nil
      val row = r("Cell 1", "Cell 2", "Cell 3")
      assertTableEquals(Table(header, colOpts, row :: Nil), comment.body)
    }

    withComment("ColumnOptionsHyphenRepetitions") { comment =>
      val header = r("First Header", "Second Header", "Third Header")
      val colOpts = ColumnOptionLeft :: ColumnOptionCenter :: ColumnOptionRight :: Nil
      assertTableEquals(Table(header, colOpts, Nil), comment.body)
    }

    withComment("HeaderConstraints") { comment =>
      val header = r("First Header", "Second Header")
      val colOpts = ColumnOptionCenter :: ColumnOptionCenter :: Nil
      val row1 = r("Pork", "Veal")
      val row2 = r("Yam", "")
      val rows = row1 :: row2 :: Nil
      assertTableEquals(Table(header, colOpts, rows), comment.body)
    }

    withComment("CellsUsingMarkdown") { comment =>
      val header = r("Edibles")
      val colOpts = ColumnOptionLeft :: Nil

      val cell1 = ci(Chain(List(Text("Oranges "), Underline(Text("and")), Text(" Aubergines"))))

      val cell2 = ci(Chain(List(Text("Peaches "), Monospace(Text("or")), Text(" Pears"))))

      val row1 = Row(cell1 :: Nil)
      val row2 = Row(cell2 :: Nil)
      val rows = row1 :: row2 :: Nil
      assertTableEquals(Table(header, colOpts, rows), comment.body)
    }

    withComment("CellsUsingMarkdownInHeader") { comment =>
      val header = {
        val cell1 = ci(Bold(Text("Nibbles")))
        val cell2 = ci(Italic(Text("Main")))
        val cell3 = ci(Monospace(Text("Desert")))
        Row(cell1 :: cell2 :: cell3 :: Nil)
      }
      val colOpts = ColumnOptionCenter :: ColumnOptionCenter :: ColumnOptionLeft :: Nil

      val row1 = r("Bread", "Yak", "Vodka")
      val row2 = {
        val cell1 = c("Figs")
        val cell2 = ci(Chain(Text("Cheese on toast") :: Superscript(Text("three ways")) :: Nil))
        val cell3 = c("Coffee")
        Row(cell1 :: cell2 :: cell3 :: Nil)
      }
      val rows = row1 :: row2 :: Nil
      assertTableEquals(Table(header, colOpts, rows), comment.body)
    }

    withComment("TrailingCellsEmpty") { comment =>
      val header = r("Header 1", "Header 2", "")
      val colOpts = ColumnOptionLeft :: ColumnOptionLeft :: ColumnOptionLeft :: Nil

      val row1 = r("Fig", "", "")
      val row2 = r("Cherry", "", "")
      val row3 = r("Walnut", "", "")
      val rows = row1 :: row2 :: row3 :: Nil
      assertTableEquals(Table(header, colOpts, rows), comment.body)
    }

    withComment("LeadingCellsEmpty") { comment =>
      val nilCell = Cell(Nil)
      val emptyCell = c("")

      val header = Row(emptyCell :: c("Header 1") :: c("Header 2") :: Nil)
      val colOpts = ColumnOptionLeft :: ColumnOptionLeft :: ColumnOptionLeft :: Nil

      val row1 = Row(emptyCell :: nilCell :: c("Fig") :: Nil)
      val row2 = Row(emptyCell :: c("Cherry") :: nilCell :: Nil)
      val row3 = Row(c("Walnut") :: nilCell :: nilCell :: Nil)
      val rows = row1 :: row2 :: row3 :: Nil
      assertTableEquals(Table(header, colOpts, rows), comment.body)
    }

    withComment("HeadersUsingInlineMarkdown") { comment =>
      val headerCell1 = ci(
        Chain(
          Text("Fruits, ") :: Subscript(Text("Beverages")) :: Text(" and Vegetables") :: Nil
        )
      )
      val headerCell2 = ci(
        Chain(
          Text("Semiconductors, ") :: Italic(Text("Raptors")) :: Text(", and Poultry") :: Nil
        )
      )

      val header = Row(headerCell1 :: headerCell2 :: Nil)
      val colOpts = ColumnOptionLeft :: ColumnOptionLeft :: Nil

      val row = r("Out of stock", "7 left")
      val rows = row :: Nil
      assertTableEquals(Table(header, colOpts, rows), comment.body)
    }

    withComment("Combined") { comment =>

      val header = r("Item", "Price")
      val colOpts = ColumnOptionLeft :: ColumnOptionRight :: Nil

      val row1 = r("Rookworst", "€ 15,00")
      val row2 = r("Apple Sauce", "€ 5,00")
      val rows = row1 :: row2 :: Nil
      assertTableEquals(Table(header, colOpts, rows), comment.body)
    }

    withComment("CellInlineMarkdown") { comment =>

      val header = r("Header")
      val colOpts = ColumnOptionLeft :: Nil

      val row = Row(ci(HtmlTag("<a href=\"tttt\">link</a>")) :: Nil)

      val rows = row :: Nil
      assertTableEquals(Table(header, colOpts, rows), comment.body)
    }

    withComment("MultipleTables1") { comment =>

      val colOpts = ColumnOptionLeft :: Nil

      val table1 = Table(r("Hill Dweller"), colOpts, r("Ant") :: Nil)
      val table2 = Table(r("Hive Dweller"), colOpts, r("Bee") :: Nil)

      assertTablesEquals(table1 :: table2 :: Nil, comment.body)
    }

    withComment("MultipleTables2") { comment =>

      val colOpts = ColumnOptionLeft :: Nil

      val table1 = Table(r("Hill Dweller"), colOpts, r("Ant") :: Nil)
      val table2 = Table(r("Hive Dweller"), colOpts, r("Bee") :: Nil)
      val table3 = Table(r("Forest Dweller"), colOpts, r("Cricket") :: Nil)

      assertTablesEquals(table1 :: table2 :: table3 :: Nil, comment.body)
    }

    {
      val colOpts = ColumnOptionLeft :: Nil

      val table1 = Table(r("Hill Dweller"), colOpts, r("Ant") :: Nil)
      val table2 = Table(r("Hive Dweller"), colOpts, r("Bee") :: Nil)

      val content1 = Paragraph(Chain(List(Summary(Chain(List(Text("Ants are cool"), Text(".")))))))
      val content2 = pt("But bees are better.\n")

      val body = Body(table1 :: content1 :: table2 :: content2 :: Nil)

      withComment("MixedContent") { comment =>
        assertBodiesEquals(body, comment.body)
      }
    }

    withComment("ParagraphEnd") { comment =>

      val summary = Paragraph(Chain(List(Summary(Text("Summary")))))
      val paragraph = pt("Paragraph text should end here.")
      val header = r("type")
      val colOpts = ColumnOptionLeft :: Nil
      val table = Table(header, colOpts, r("nuttiest") :: Nil)
      val expected = Body(List(summary, paragraph, table))

      assertBodiesEquals(expected, comment.body)
    }

    withComment("CellMarkerEscaped") { comment =>
      val header = r("First |Header", "Second| Header", "Third|Head\\er")
      val colOpts = ColumnOptionCenter :: ColumnOptionLeft :: ColumnOptionRight :: Nil

      val row1 = r("a|b", "cd", "ef")
      val row2 = r("|Content 1", "", "")
      val row3 = r("C|ontent 2", "", "")
      val row4 = r("Content| 3", "", "")
      val row5 = r("Content  |4", "||", "||||")
      val row6 = Row(Cell(List(Paragraph(Text("Content 5|")))) :: Cell(Nil) :: Cell(Nil) :: Nil)

      val rows = row1 :: row2 :: row3 :: row4 :: row5 :: row6 :: Nil
      assertTableEquals(Table(header, colOpts, rows), comment.body)
    }

    withComment("CellMarkerEscapeEscapesOnlyMarker") { comment =>
      val header = r("Domain", "Symbol", "Operation", "Extra")
      val colOpts = ColumnOptionLeft :: ColumnOptionCenter :: ColumnOptionLeft :: ColumnOptionLeft :: Nil

      val row1 = r("Bitwise", " | ", "Or", "")
      val row2 = r("Strange", raw"|\|", "???", raw"\N")

      val rows = row1 :: row2 :: Nil
      assertTableEquals(Table(header, colOpts, rows), comment.body)
    }

    withComment("MissingInitialCellMark") { comment =>

      val colOpts = ColumnOptionLeft :: Nil

      val table1 = Table(r("Unstarted Row"), colOpts, r("r1c1") :: Nil)

      val content = Paragraph(Chain(List(Summary(Text("r2c1|")))))

      val table2 = Table(r("r3c1"), colOpts, Nil)

      val body = Body(table1 :: content :: table2 :: Nil)

      assertBodiesEquals(body, comment.body)
    }

    withComment("SplitCellContent") { comment =>
      val header = r("Split")
      val colOpts = ColumnOptionLeft :: Nil

      val table = Table(header, colOpts, Nil)

      val content = Paragraph(Chain(List(Summary(Text("|Accidental\nnewline|")))))

      val body = Body(table :: content :: Nil)

      assertBodiesEquals(body, comment.body)
    }

    withComment("SplitInternalCellContent") { comment =>
      val colOpts = ColumnOptionLeft :: Nil

      val table1 = Table(r("Split"), colOpts, Nil)

      val content = Paragraph(Chain(List(Summary(Text("|Accidental\nnewline|")))))

      val table2 = Table(r("~FIN~"), colOpts, Nil)

      val body = Body(table1 :: content :: table2 :: Nil)

      assertBodiesEquals(body, comment.body)
    }

    withComment("MixedContentUnspaced") { comment =>
      val colOpts = ColumnOptionLeft :: Nil

      val table1 = Table(r("Hill Dweller"), colOpts, r("Ant") :: Nil)

      val content1 = Paragraph(Chain(List(Summary(Text("Ants are cool")))))

      val table2 = Table(r("Hive Dweller"), colOpts, r("Bee") :: Nil)

      val content2 = pt("But bees are better.\n")

      val body = Body(table1 :: content1 :: table2 :: content2 :: Nil)

      assertBodiesEquals(body, comment.body)
    }

    /* Deferred Enhancements.
     *
     * When these improvements are made corresponding test updates to any new or
     * changed error messages and parsed content and would be included.
     */

    // As a later enhancement skip whitespace before table marks to reduce rate of silently ignored intended table markdown.
    /* Confirm current suboptimal behaviour */
    withComment("LeadingWhitespaceNotSkipped") { comment =>
      val colOpts = ColumnOptionLeft :: Nil
      val table = Table(r("Leading"), colOpts, Nil)
      val text = " |-|\n  |whitespace before marks|\n   |Not Yet Skipped|Maybe TO DO|\n"
      val content = Paragraph(Chain(List(Summary(Text(text)))))

      val body = Body(table :: content :: Nil)
      assertBodiesEquals(body, comment.body)
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

  private def assertTablesEquals(expectedTables: Seq[Table], actualBody: Body): Unit = {
    val expectedBody = Body(expectedTables)
    assert(expectedBody == actualBody, s"Expected: $expectedBody, Actual: $actualBody")
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