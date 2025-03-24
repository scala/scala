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

package scala.annotation.meta

/**
 * When defining an implicit class, the Scala compiler creates an implicit
 * conversion method for it. Annotations `@companionClass` and `@companionMethod`
 * control where an annotation on the implicit class will go. By default, annotations
 * on an implicit class end up only on the class.
 *
 */
final class companionMethod extends scala.annotation.StaticAnnotation
