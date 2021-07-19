// scalac: -Xfatal-warnings

sealed trait SealedTrait

class SomeClass

trait UnsealedTrait

sealed abstract class O

class O1 extends O

final class Foo

class Bar

class Test {

  // final class, sealed trait
  def a(c: Option[Int]) = c match { case _: SealedTrait => ; case _ => }

  // final class, SomeClass
  def b(c: Option[Int]) = c match { case _: SomeClass => ; case _  => }

  // final class, trait, Can't we check it?
  def c(c: Option[Int]) = c match { case _: UnsealedTrait => ; case _ => }

  def d() = Map("outer" -> Map("inner" -> new Foo)).collect { case (_, foo: Foo) => "should error" }

  // if Foo is not final, not warn
  def e() = Map(1 -> new Foo).collect { case (_, bar: Bar) => "should error" }

  // two errors
  def f(c: O1) = Map("outer" -> Map("inner" -> new Foo)).collect { case (_: Int, foo: Foo) => "should error" }

  // O1 is not final , so there could be a value of type O1 with UnsealedTrait
  // Because maybe exists `O2 extends O1 with UnsealedTrait` ?
  def nowarn1(c: O) = c match {
    case _: UnsealedTrait =>
    case _                =>
  }

  def nowarn2(c: O1) = c match {
    case _: UnsealedTrait =>
    case _                =>
  }

  def nowarn3(c: O1) = Map("outer" -> Map("inner" -> new Foo)).collect { case (_, o: O1) => }
}