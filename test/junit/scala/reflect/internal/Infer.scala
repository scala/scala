package scala.reflect.internal

import org.junit.Assert._
import org.junit.{After, Assert, Before, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.collection.mutable
import scala.tools.nsc.settings.ScalaVersion
import scala.tools.nsc.symtab.SymbolTableForUnitTesting
import language.higherKinds

import scala.tools.testkit.BytecodeTesting

@RunWith(classOf[JUnit4])
class InferTest extends BytecodeTesting {
  import compiler.global._, definitions._, analyzer._, typer.infer._

  class D[T]
  class A

  type DA = D[A]

  class C[F[_]](val i: Int)

  object Foo {
    implicit def foo0[F[_]]: C[F] = ???
    implicit def foo1: C[D] = ???
  }

  @Test
  def testContains(): Unit = {
    val run = new global.Run

    enteringPhase(run.typerPhase) {
      val asym = symbolOf[A]
      val dsym = symbolOf[D[_]]

      val tp0 = typeOf[D[A]]
      assert(tp0.contains(asym))

      val tp2 = typeOf[C[D]]
      assert(tp2.contains(dsym))

      val foo0Sym = typeOf[Foo.type].member(TermName("foo0"))
      val foo0Tpe = foo0Sym.info

      val PolyType(List(fSym), NullaryMethodType(restpe0)) = foo0Tpe
      assert(restpe0.contains(fSym))

      // existentialAbstraction uses contains
      val abstracted = existentialAbstraction(List(fSym), restpe0)
      val expected = ExistentialType(List(fSym), restpe0)
      assert(abstracted == expected)

      val foo1Sym = typeOf[Foo.type].member(TermName("foo1"))
      val foo1Tpe = foo1Sym.info

      // isStrictlyMoreSpecific uses existentialAbstraction
      assert(!isStrictlyMoreSpecific(foo0Tpe, foo1Tpe, foo0Sym, foo1Sym))
      assert(isStrictlyMoreSpecific(foo1Tpe, foo0Tpe, foo1Sym, foo0Sym))
    }
  }
}
