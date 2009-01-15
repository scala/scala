package scalax.util

import java.beans.Introspector

/**
 * @author ilyas
 */

object StringUtil {

  def trimStart(s: String, prefix: String) = if (s != null && s.startsWith(prefix)) s.substring(prefix.length) else s

  def decapitalize(s: String) = Introspector.decapitalize(s)

}