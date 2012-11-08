/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package io

import ch.epfl.lamp.compiler.msil.{ Type => MsilType }

/** This class wraps an MsilType.  It exists only so
 *  ClassPath can treat all of JVM/MSIL/bin/src files
 *  uniformly, as AbstractFiles.
 */
class MsilFile(val msilType: MsilType) extends VirtualFile(msilType.FullName, msilType.Namespace) {
}

object NoMsilFile extends MsilFile(null) { }
