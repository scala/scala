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

package scala
package tools.ant.sabbus

import org.apache.tools.ant.Task

class Break extends Task {

  def setId(input: String) {
    id = Some(input)
  }

  private var id: Option[String] = None

  override def execute() {
    if (id.isEmpty) sys.error("Attribute 'id' is not set")
    Compilers.break(id.get)
  }

}
