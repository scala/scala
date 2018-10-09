package scala.test.scaladoc.tables {

  /**
    * |First Header|
    * |---|
    * |Content Cell|
    */
  trait Minimal

  /**
    * |No Data Rows|
    * |---|
    */
  trait NoDataRows

  /**
    * |First Header|Second Header|Third Header|
    * |:---|:---:|---:|
    * |Cell 1|Cell 2|Cell 3|
    */
  trait ColumnOptionsAllTypes

  /**
    * |First Header|Second Header|Third Header|
    * |:----|:-----:|------:|
    * |Cell 1|Cell 2|Cell 3|
    */
  trait ColumnOptionsMoreThanThreeHyphens

  /**
    * |First Header|Second Header|Third Header|
    * |-|:--:|---:|
    */
  trait ColumnOptionsHyphenRepetitions

  /**
    * |First Header|Second Header|
    * |:---:|:---:|----|
    * |Pork|Veal|Yak|
    * |Yam|
    *
    */
  trait HeaderConstraints

  /**
    * |Edibles|
    * |---|
    * |Oranges __and__ Aubergines|
    * |Peaches `or` Pears|
    */
  trait CellsUsingMarkdown

  /**
    * |'''Nibbles'''|''Main''|`Desert`|
    * |:--:|:---:|----|
    * |Bread|Yak|Vodka|
    * |Figs|Cheese on toast^three ways^|Coffee|
    */
  trait CellsUsingMarkdownInHeader

  /**
    * |Header 1|Header 2||
    * |---|---|---|
    * |Fig||
    * |Cherry|||
    * |Walnut|
    */
  trait TrailingCellsEmpty

  /**
    * ||Header 1|Header 2|
    * |---|---|---|
    * |||Fig|
    * ||Cherry||
    * |Walnut|||
    */
  trait LeadingCellsEmpty

  // Headers

  /**
    * |Fruits, ,,Beverages,, and Vegetables|Semiconductors, ''Raptors'', and Poultry|
    * |---|---|
    * |Out of stock|7 left|
    */
  trait HeadersUsingInlineMarkdown

  /**
    * |Item|Price|
    * |---|---:|
    * |Rookworst|€ 15,00|
    * |Apple Sauce|€ 5,00|
    */
  trait Combined

  /**
    * |Header|
    * |---|
    * |<a href="tttt">link</a>|
    */
  trait CellInlineMarkdown

  /**
    * |Hill Dweller|
    * |---|
    * |Ant|
    *
    * |Hive Dweller|
    * |---|
    * |Bee|
    *
    */
  trait MultipleTables1

  /**
    * |Hill Dweller|
    * |---|
    * |Ant|
    *
    * |Hive Dweller|
    * |---|
    * |Bee|
    *
    * |Forest Dweller|
    * |---|
    * |Cricket|
    *
    */
  trait MultipleTables2

  /**
    * |Hill Dweller|
    * |---|
    * |Ant|
    *
    * Ants are cool.
    *
    * |Hive Dweller|
    * |---|
    * |Bee|
    *
    * But bees are better.
    */
  trait MixedContent

  /**
    * Summary
    *
    * Paragraph text should end here.
    * |type|
    * |-|
    * |nuttiest|
    */
  trait ParagraphEnd

  /**
    * |First \|Header|Second\| Header|Third\|Head\er|
    * |:---:|:---|-:|
    * |a\|b|cd|ef|
    * |\|Content 1|||
    * |C\|ontent 2|||
    * |Content\| 3|||
    * |Content  \|4|\|\||\|\|\|\||
    * |Content 5\||
    */
  trait CellMarkerEscaped

  /**
    * |Domain|Symbol|Operation|Extra|
    * |---|:---:|---|---|
    * |Bitwise| \| |Or||
    * |Strange|\|\\||???|\N|
    */
  trait CellMarkerEscapeEscapesOnlyMarker

  /**
    * |Unstarted Row|
    * |-|
    * |r1c1|
    * r2c1|
    * |r3c1|
    *
    */
  trait MissingInitialCellMark

  /**
    * |Split|
    * |-|
    * |Accidental
    * newline|
    *
    */
  trait SplitCellContent

  /**
    * |Split|
    * |-|
    * |Accidental
    * newline|
    * |~FIN~|
    *
    */
  trait SplitInternalCellContent

  /**
    * |Hill Dweller|
    * |---|
    * |Ant|
    * Ants are cool
    * |Hive Dweller|
    * |---|
    * |Bee|
    * But bees are better.
    */
  trait MixedContentUnspaced

  // Known suboptimal behaviour. Candidates for improving later.

  // Because table rows must not have leading whitespace this
  // should parse to a table with a header, defaulted delimiter and no rows
  // while the ignored content is parsed as non-table content.
  /**
    * |Leading|
    *  |-|
    *   |whitespace before marks|
    *    |Not Yet Skipped|Maybe TO DO|
    */
  trait LeadingWhitespaceNotSkipped

}