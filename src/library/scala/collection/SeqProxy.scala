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

/** This trait implements a proxy for sequence objects. It forwards
 *  all calls to a different sequence object.
 *
 *  @author  Martin Odersky
 *  @since   2.8
 */
@deprecated("proxying is deprecated due to lack of use and compiler-level support", "2.11.0")
trait SeqProxy[+A] extends Seq[A] with SeqProxyLike[A, Seq[A]]
