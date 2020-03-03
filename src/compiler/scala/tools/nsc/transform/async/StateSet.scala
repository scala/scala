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

package scala.tools.nsc.transform.async

import java.util
import java.util.function.{Consumer, IntConsumer}

import scala.collection.JavaConverters.{asScalaIteratorConverter, iterableAsScalaIterableConverter}

// Set for StateIds, which are either small positive integers or -symbolID.
final class StateSet {
  private val bitSet = new java.util.BitSet()
  private val caseSet = new util.HashSet[Integer]()
  private def useBitSet(i: Int) = i > 0 && i < 1024
  def +=(stateId: Int): Unit = if (useBitSet(stateId)) bitSet.set(stateId) else caseSet.add(stateId)
  def -=(stateId: Int): Unit = if (useBitSet(stateId)) bitSet.clear(stateId) else caseSet.remove(stateId)
  def contains(stateId: Int): Boolean = if (useBitSet(stateId)) bitSet.get(stateId) else caseSet.contains(stateId)
  def isEmpty = bitSet.isEmpty && caseSet.isEmpty
  def iterator: Iterator[Integer] = {
    bitSet.stream().iterator().asScala ++ caseSet.asScala.iterator
  }
  def toArray: Array[Int] = {
    val result = new Array[Int](bitSet.cardinality() + caseSet.size())
    var i = 0
    foreach(value => {result(i) = value; i += 1 })
    result
  }
  def foreach(f: IntConsumer): Unit = {
    bitSet.stream().forEach(f)
    caseSet.stream().forEach(new Consumer[Integer] {
      override def accept(value: Integer): Unit = f.accept(value)
    })
  }
}
