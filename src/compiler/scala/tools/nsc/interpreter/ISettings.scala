/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Alexander Spoon
 */

package scala.tools.nsc
package interpreter

/** Settings for the interpreter
 *
 * @version 1.0
 * @author Lex Spoon, 2007/3/24
 **/
class ISettings(intp: IMain) {
  /** The maximum length of toString to use when printing the result
   *  of an evaluation.  0 means no maximum.  If a printout requires
   *  more than this number of characters, then the printout is
   *  truncated.
   */
  var maxPrintString = replProps.maxPrintString.option.getOrElse(800)

  /** The maximum number of completion candidates to print for tab
   *  completion without requiring confirmation.
   */
  var maxAutoprintCompletion = 250

  /** String unwrapping can be disabled if it is causing issues.
   *  Setting this to false means you will see Strings like "$iw.$iw.".
   */
  var unwrapStrings = true

  def deprecation_=(x: Boolean) = {
    val old = intp.settings.deprecation.value
    intp.settings.deprecation.value = x
    if (!old && x) println("Enabled -deprecation output.")
    else if (old && !x) println("Disabled -deprecation output.")
  }
  def deprecation: Boolean = intp.settings.deprecation.value

  def allSettings = Map[String, Any](
    "maxPrintString" -> maxPrintString,
    "maxAutoprintCompletion" -> maxAutoprintCompletion,
    "unwrapStrings" -> unwrapStrings,
    "deprecation" -> deprecation
  )

  private def allSettingsString =
    allSettings.toList sortBy (_._1) map { case (k, v) => "  " + k + " = " + v + "\n" } mkString

  override def toString = """
    | ISettings {
    | %s
    | }""".stripMargin.format(allSettingsString)
}
