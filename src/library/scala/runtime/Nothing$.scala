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

package scala
package runtime


/**
 * Dummy class which exist only to satisfy the JVM. It corresponds
 * to `scala.Nothing`. If such type appears in method
 * signatures, it is erased to this one.
 */
sealed abstract class Nothing$ extends Throwable
