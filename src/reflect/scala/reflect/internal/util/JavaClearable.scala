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

package scala.reflect.internal.util

import java.lang.ref.WeakReference
import java.util.{Collection => JCollection, Map => JMap}

import scala.collection.mutable.Clearable

object JavaClearable {
  def forCollection[T <: JCollection[_]](data: T): JavaClearable[T] = new JavaClearableCollection(new WeakReference(data))
  def forMap[T <: JMap[_,_]](data: T): JavaClearable[T] = new JavaClearableMap(new WeakReference(data))

  private final class JavaClearableMap[T <: JMap[_,_]](dataRef:WeakReference[T]) extends JavaClearable(dataRef) {
    override def clear() = Option(dataRef.get).foreach(_.clear())
  }
  private final class JavaClearableCollection[T <: JCollection[_]](dataRef:WeakReference[T]) extends JavaClearable(dataRef) {
    override def clear() = Option(dataRef.get).foreach(_.clear())
  }
}
sealed abstract class JavaClearable[T <: AnyRef] protected (protected val dataRef: WeakReference[T]) extends Clearable {

  //just maintained hashCode to be consistent with equals
  override val hashCode = System.identityHashCode(dataRef.get())
  override def equals(obj: scala.Any) = obj match {
    case that: JavaClearable[_] => {
      if (this eq that) true
      else {
        val thisData = this.dataRef.get
        val thatData = that.dataRef.get
        (thisData eq thatData) && (thisData ne null)
      }
    }
    case _ => false
  }

  def clear(): Unit

  def isValid = dataRef.get() ne null
}
