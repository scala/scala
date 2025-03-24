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
 * An annotation giving particulars for a language feature in object `scala.language`.
 */
final class languageFeature(feature: String, enableRequired: Boolean) extends scala.annotation.StaticAnnotation
