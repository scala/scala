/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

import scalac.Global;
import scala.tools.scaladoc.TypeIsomorphism;
import scala.tools.scaladoc.ScalaML;

package scala.tools.scaladoc {

  class HTMLGeneratorScala(global: Global) extends HTMLGenerator(global) {
    def newTypeIso(global: Global): TypeIsomorphism = new ScalaML(global);
  }

}
