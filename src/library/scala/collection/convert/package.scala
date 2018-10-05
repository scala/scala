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
package collection

package object convert {
  @deprecated("use JavaConverters", since="2.12.0")
  val decorateAsJava  = new DecorateAsJava { }
  @deprecated("use JavaConverters", since="2.12.0")
  val decorateAsScala = new DecorateAsScala { }
  @deprecated("use JavaConverters", since="2.12.0")
  val decorateAll     = JavaConverters

  @deprecated("use JavaConverters or consider ImplicitConversionsToJava", since="2.12.0")
  val wrapAsJava      = new WrapAsJava { }
  @deprecated("use JavaConverters or consider ImplicitConversionsToScala", since="2.12.0")
  val wrapAsScala     = new WrapAsScala { }
  @deprecated("use JavaConverters or consider ImplicitConversions", since="2.12.0")
  val wrapAll         = new WrapAsJava with WrapAsScala { }
}
