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
 * Dummy class which exist only to satisfy the JVM. It corresponds to
 * `scala.Null`. If such type appears in method signatures, it is erased
 * to this one. A private constructor ensures that Java code can't create
 * subclasses. The only value of type Null$ should be null
 */
sealed abstract class Null$ private ()
