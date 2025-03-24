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

package scala.annotation

/**
 * A base class for annotations.
 *
 * Annotations extending this class directly are not preserved in the classfile. To enable storing
 * annotations in the classfile's Scala signature and make it available to Scala reflection and
 * other tools, the annotation needs to inherit from [[scala.annotation.StaticAnnotation]].
 *
 * Annotation classes defined in Scala are not stored in classfiles in a Java-compatible manner
 * and therefore not visible in Java reflection. In order to achieve this, the annotation has to
 * be written in Java.
 */
abstract class Annotation
