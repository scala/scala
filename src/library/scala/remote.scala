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

/**
 * An annotation that designates the class to which it is applied as remotable.
 *
 * For instance, the Scala code
 * {{{
 * @remote trait Hello {
 *   def sayHello(): String
 * }
 * }}}
 * is equivalent to the following Java code:
 * {{{
 * public interface Hello extends java.rmi.Remote {
 *     String sayHello() throws java.rmi.RemoteException;
 * }
 * }}}
 */
@deprecated("extend java.rmi.Remote instead and add @throws[java.rmi.RemoteException] to public methods", "2.12.0")
class remote extends scala.annotation.StaticAnnotation {}
