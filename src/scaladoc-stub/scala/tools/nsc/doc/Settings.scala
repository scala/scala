package scala.tools.nsc
package doc

import java.io.File
import scala.language.postfixOps

class Settings(error: String => Unit, val printMsg: String => Unit = println(_)) extends scala.tools.nsc.Settings(error)
