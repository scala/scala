//> using options -Xasync

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.tools.testkit.async.Async._
import Future.{successful => f}


class NakedAwait {
  def `await only allowed in async neg`(): Unit = {
     await[Any](f(null))
  }

  def `await not allowed in by-name argument`(): Unit = {
    // expectError("await must not be used under a by-name argument.") {
    def foo(a: Int)(b: => Int) = 0
    async { foo(0)(await(f(0))) }
  }

  def nestedObject(): Unit = {
    // expectError("await must not be used under a nested object.") {
    async { object Nested { await(f(false)) } }
  }

  def nestedTrait(): Unit = {
    // expectError("await must not be used under a nested trait.") {
    async { trait Nested { await(f(false)) } }
  }

  def nestedClass(): Unit = {
    // expectError("await must not be used under a nested class.") {
    async { class Nested { await(f(false)) } }
  }

  def nestedFunction(): Unit = {
    // expectError("await must not be used under a nested function.") {
    async { () => { await(f(false)) } }
  }

  def nestedPatMatFunction(): Unit = {
    // expectError("await must not be used under a nested class.") { // TODO more specific error message
    async { { case 0 => { await(f(false)) } } : PartialFunction[Int, Boolean] }
  }

  def tryBody(): Unit = {
    // expectError("await must not be used under a try/catch.") {
    async { try { await(f(false)) } catch { case _: Throwable => } }
  }

  def catchBody(): Unit = {
    // expectError("await must not be used under a try/catch.") {
    async { try { () } catch { case _: Throwable => await(f(false)) } }
  }

  def finallyBody(): Unit = {
    // expectError("await must not be used under a try/catch.") {
    async { try { () } finally { await(f(false)) } }
  }

  def nestedMethod(): Unit = {
    // expectError("await must not be used under a nested method.") {
    async { def foo = await(f(false)) }
  }

//  def returnIllegal(): Unit = {
//    def foo(): Any = async { return false } //!!!
//  }

  def lazyValIllegal(): Unit = {
    //expectError("await must not be used under a lazy val initializer")
    def foo(): Any = async { val x = { lazy val y = await(f(0)); y } }
  }

  def byNameFunction(): Unit = {
    def foo(x: String)(a: => String): String = a.reverse
    def fooAsByNameLambda = foo("") _ // : (_ => String) = String
    async { fooAsByNameLambda(await(f(""))) }
  }

  def underSynchronized(): Unit = {
    val lock = new Object
    async { lock.synchronized { await(f(1)) + await(f(2)) } }
  }
}
