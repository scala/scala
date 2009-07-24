package scala.tools.scalap
package scalax
package util

import java.beans.Introspector

/**
 * @author ilyas
 */

object StringUtil {

  def trimStart(s: String, prefix: String) = if (s != null && s.startsWith(prefix)) s.substring(prefix.length) else s

  def decapitalize(s: String) = Introspector.decapitalize(s)

  def cutSubstring(dom: String)(s: String) = if (dom != null && s != null) dom.replace(s, "") else dom

}
