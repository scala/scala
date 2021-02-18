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
package mutable

/** Class for the linked hash map entry, used internally.
 *  @since 2.8
 */
@SerialVersionUID(-2671939643954900582L)
final class LinkedEntry[A, B](val key: A, var value: B)
      extends HashEntry[A, LinkedEntry[A, B]] with Serializable {
  var earlier: LinkedEntry[A, B] = null
  var later: LinkedEntry[A, B] = null
}

