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

package scala.runtime

import java.lang.invoke._
import java.util

import scala.annotation.varargs
import scala.collection.immutable

final class LambdaDeserialize private (lookup: MethodHandles.Lookup, targetMethods: Array[MethodHandle]) {
  private val targetMethodMap: util.HashMap[String, MethodHandle] = new util.HashMap[String, MethodHandle](targetMethods.length)

  for (targetMethod <- targetMethods) {
    val info = lookup.revealDirect(targetMethod)
    val key = LambdaDeserialize.nameAndDescriptorKey(info.getName, info.getMethodType.toMethodDescriptorString)
    targetMethodMap.put(key, targetMethod)
  }

  private val cache = new util.HashMap[String, MethodHandle]

  def deserializeLambda(serialized: SerializedLambda): AnyRef = LambdaDeserializer.deserializeLambda(lookup, cache, targetMethodMap, serialized)
}

object LambdaDeserialize {
  @varargs @throws[Throwable]
  def bootstrap(lookup: MethodHandles.Lookup, invokedName: String, invokedType: MethodType, targetMethods: MethodHandle*): CallSite = {
    val targetMethodsArray = targetMethods.asInstanceOf[immutable.ArraySeq[_]].unsafeArray.asInstanceOf[Array[MethodHandle]]
    val exact = MethodHandleConstants.LAMBDA_DESERIALIZE_DESERIALIZE_LAMBDA.bindTo(new LambdaDeserialize(lookup, targetMethodsArray)).asType(invokedType)
    new ConstantCallSite(exact)
  }

  def nameAndDescriptorKey(name: String, descriptor: String): String = name + descriptor
}
