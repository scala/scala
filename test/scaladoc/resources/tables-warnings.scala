package scala.test.scaladoc.tables.warnings {

  /**
    * |Header|
    * |-|
    * |cell*/
  trait PrematureEndOfText

  /**
    * |Unterminated|
    * |-|
    * |r1c1|
    * |r2c1
    * |r3c1|
    *
    */
  trait MissingTrailingCellMark

  /**
    * |colon-colon|middle-colon|random|center|
    * |::-|-:-|??|:----------------:|
    * |a|b|c|d|
    * */
  trait InvalidColumnOptions

  /**
    * |Sequence|
    * |''---''|
    * |9|
    * */
  trait InvalidMarkdownUsingColumnOptions

}