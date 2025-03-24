/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package ast.parser

/** A descriptor for a matching pair of braces.
 *  @param loff    The offset of the opening brace (-1 means missing)
 *  @param lindent The indentation depth of the line of the opening brace (-1 means missing)
 *  @param roff    The offset of the closing brace (-1 means missing)
 *  @param rindent The indentation depth of the line of the closing brace (-1 means missing)
 *  @param nested The brace pairs nested in this one
 */
case class BracePair(loff: Int, lindent: Int, roff: Int, rindent: Int, nested: List[BracePair])


