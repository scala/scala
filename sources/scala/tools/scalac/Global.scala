/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id: Global.scala
\*                                                                      */

import scalac.{CompilerCommand, Global => scalac_Global};
import scalac.ast.printer.TreePrinter;

package scala.tools.scalac {

import ast.printer._;
import java.io.OutputStream;

/** The global environment of a compiler run
 *
 */
class Global(args: CompilerCommand, interpret: boolean) extends scalac_Global(args, interpret) {

  def this(args: CompilerCommand) = this(args, false);

  protected override def newTextTreePrinter(printStream: OutputStream): TreePrinter =
    new TextTreePrinter(printStream);
  protected override def newHTMLTreePrinter(printStream: OutputStream): TreePrinter =
    new HTMLTreePrinter(printStream);

}
}
