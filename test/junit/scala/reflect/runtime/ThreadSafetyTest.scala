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

package scala.reflect.runtime

import java.util.concurrent.{Callable, Executors}

import org.junit.Test

class ThreadSafetyTest {
  import scala.reflect.runtime.universe._
  sealed abstract class Instance(tp: Type)
  final case class ListInstance(tp: Type, elemInstance: Instance) extends Instance(appliedType(symbolOf[List[_]], tp :: Nil))

  final case class DoubleInstance() extends Instance(typeOf[Double])

  final case class StringInstance() extends Instance(typeOf[String])

  final case class LowPriorityInstance(tpe: Type) extends Instance(tpe)

  def classKeyOf[T: TypeTag]: String = {
    classKeyOf(typeOf[T])
  }
  def classKeyOf(tpe: Type): String = {
    tpe.typeSymbol.fullName
  }

  class Lazy[T](thunk: () => T) {
    lazy val force: T = thunk()
  }

  class Registry {
    private val cache = new java.util.concurrent.ConcurrentHashMap[Type, Lazy[Instance]]()
    def instance[T: TypeTag]: Lazy[Instance] = {
      instance(typeOf[T])
    }
    def instance(tpe: Type): Lazy[Instance] = {
      val value = keyOf(tpe)
      assert(value =:= tpe, (value, tpe))
      cache.computeIfAbsent(value, create(_))
    }

    private def create(tpe: Type): Lazy[Instance] = {
      val key = classKeyOf(tpe)
      if (key == "scala.collection.immutable.List") {
        new Lazy(() => {val elemTpe = tpe.dealias.typeArgs.head; ListInstance(tpe.dealias, instance(elemTpe).force)})
      } else if (key == "scala.Double") {
        new Lazy(() => DoubleInstance())
      } else if (key == "java.lang.String") {
        new Lazy(() => StringInstance())
      } else {
        new Lazy(() => LowPriorityInstance(tpe))
      }
    }
    private def keyOf(tp: Type): universe.Type = {
      tp.map(_.dealias)
    }
  }

  @Test
  def test(): Unit = {
    val executor = Executors.newFixedThreadPool(16)
    for (i <- (0 to 128)) {
      val registry = new Registry
      val is = List(
        (() => typeOf[List[Double]], "ListInstance(List[Double],DoubleInstance())"),
        (() => typeOf[List[String]], "ListInstance(List[String],StringInstance())"),
        (() => typeOf[List[List[Double]]], "ListInstance(List[List[Double]],ListInstance(List[Double],DoubleInstance()))"),
        (() => typeOf[List[List[List[Double]]]], "ListInstance(List[List[List[Double]]],ListInstance(List[List[Double]],ListInstance(List[Double],DoubleInstance())))"),
        (() => typeOf[List[List[List[Object]]]], "ListInstance(List[List[List[Object]]],ListInstance(List[List[Object]],ListInstance(List[Object],LowPriorityInstance(Object))))")
      )
      sealed abstract class Result
      case object Okay extends Result
      case class Failed(i: Int, tp: Type, expected: String, instance: String) extends Result
      def check(i: Int): Result = {
        val (f, expected) = is(i % is.size)
        val tp = f()
        val instance = registry.instance(tp).force
        if (instance.toString == expected) Okay else Failed(i, tp, expected, "[" + instance + "]")
      }
      val par = true
      val fs: List[Result] = if (par) Array.tabulate(32)(i =>
        executor.submit(new Callable[Result] {
          override def call(): Result = {
            check(i % is.size)
          }
        })).map(_.get).toList
      else is.indices.map(check).toList
      val fails = fs.filter(_ != Okay)
      assert(fails.isEmpty, "iteration " + i + ": " + fails.mkString("\n"))
    }
    executor.shutdownNow()
  }
}
