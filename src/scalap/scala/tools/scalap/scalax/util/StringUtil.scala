package scala.tools.scalap
package scalax
package util

import java.beans.Introspector

/**
 * @author ilyas
 */

object StringUtil {

  def decapitalize(s: String) = Introspector.decapitalize(s)

  def cutSubstring(dom: String)(s: String) = if (dom != null && s != null) dom.replace(s, "") else dom

}
