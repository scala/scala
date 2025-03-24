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
package reflect
package api

/**
 *  <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  A refinement of [[scala.reflect.api.Universe]] for runtime reflection using JVM classloaders.
 *
 *  This refinement equips mirrors with reflection capabilities for the JVM. `JavaMirror` can
 *  convert Scala reflection artifacts (symbols and types) into Java reflection artifacts (classes)
 *  and vice versa. It can also perform reflective invocations (getting/setting field values,
 *  calling methods, etc).
 *
 *  See the [[https://docs.scala-lang.org/overviews/reflection/overview.html Reflection Guide]] for details on how to use runtime reflection.
 *
 *  @groupname JavaUniverse Java Mirrors
 *  @group ReflectionAPI
 *
 *  @contentDiagram hideNodes "*Api"
 */
trait JavaUniverse extends Universe { self =>

  /** In runtime reflection universes, runtime representation of a class is `java.lang.Class`.
   *  @group JavaMirrors
   */
  type RuntimeClass = java.lang.Class[_]
  implicit val RuntimeClassTag: ClassTag[RuntimeClass] = ClassTag[RuntimeClass](classOf[RuntimeClass])

  /** In runtime reflection universes, mirrors are `JavaMirrors`.
   *  @group JavaMirrors
   */
  override type Mirror >: Null <: JavaMirror

  /** A refinement of [[scala.reflect.api.Mirror]] for runtime reflection using JVM classloaders.
   *
   *  With this upgrade, mirrors become capable of converting Scala reflection artifacts (symbols and types)
   *  into Java reflection artifacts (classes) and vice versa. Consequently, refined mirrors
   *  become capable of performing reflective invocations (getting/setting field values, calling methods, etc).
   *
   *  For more information about `Mirrors`s, see [[scala.reflect.api.Mirrors]] or the
   * [[https://docs.scala-lang.org/overviews/reflection/environment-universes-mirrors.html Reflection Guide: Mirrors]]
   *
   *  @group JavaMirrors
   */
  trait JavaMirror extends scala.reflect.api.Mirror[self.type] with RuntimeMirror {
    val classLoader: ClassLoader
    override def toString = s"JavaMirror with ${runtime.ReflectionUtils.show(classLoader)}"
  }

  /** Creates a runtime reflection mirror from a JVM classloader.
   *
   *  For more information about `Mirrors`s, see [[scala.reflect.api.Mirrors]] or the
   * [[https://docs.scala-lang.org/overviews/reflection/environment-universes-mirrors.html Reflection Guide: Mirrors]]
   *
   *  @group JavaMirrors
   */
  def runtimeMirror(cl: ClassLoader): Mirror
}
