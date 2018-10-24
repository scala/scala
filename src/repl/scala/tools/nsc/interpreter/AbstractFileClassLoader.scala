/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package interpreter

import scala.reflect.io.AbstractFile

@deprecated("Use `scala.tools.nsc.util.AbstractFileClassLoader`", "2.11.0")
class AbstractFileClassLoader(root: AbstractFile, parent: ClassLoader) extends util.AbstractFileClassLoader(root, parent)
