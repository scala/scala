/**
 * @define MacroWithNewLine
 * This macro should not include "*".
 *
 * @define MacroWithoutNewLine This macro
 * should
 * include "*".
 */
class Test

/**
 * TestA class
 */
class Trac4452 extends Test {
  /** $MacroWithNewLine */
  def a = 1

  /** $MacroWithoutNewLine */
  def b = 2

  /**
   * $MacroWithNewLine
   */
  def c = 3

  /**
   * $MacroWithoutNewLine
   */
  def d = 4
}
