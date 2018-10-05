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

package scala.beans

/** Provides some simple runtime processing necessary to create
 *  JavaBean descriptors for Scala entities. The compiler creates
 *  subclasses of this class automatically when the BeanInfo annotation is
 *  attached to a class.
 *
 *  @author Ross Judson (rjudson@managedobjects.com)
 */
@deprecated(message = "the generation of BeanInfo classes is no longer supported", since = "2.12.5")
abstract class ScalaBeanInfo(clazz: java.lang.Class[_],
                             props: Array[String],
                             methods: Array[String]) extends java.beans.SimpleBeanInfo {

  import java.beans._

  private val pd = new Array[PropertyDescriptor](props.length / 3)
  private val md =
    for (m <- clazz.getMethods if methods.exists(_ == m.getName))
      yield new MethodDescriptor(m)

  init()

  override def getPropertyDescriptors() = pd
  override def getMethodDescriptors() = md

  // override def getAdditionalBeanInfo() = Array(Introspector getBeanInfo clazz.getSuperclass)

  private def init() {
    var i = 0
    while (i < props.length) {
      pd(i/3) = new PropertyDescriptor(props(i), clazz, props(i+1), props(i+2))
      i = i + 3
    }
  }

}

