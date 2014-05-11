package test

// takes > 50s and > 800M heap to compile under 2.11.0
import foobar._
class `SI-8531` {
    //https://issues.scala-lang.org/browse/SI-8531

  import MyEnum._
  def foo(e1: MyEnum, e2: MyEnum) = (e1, e2) match {
    case (A1, x) => "a1"
    case (x, A1) => "a1"
    case (A2, x) => "a2"
    case (x, A2) => "a2"
    case (A3, x) => "a3"
    case (x, A3) => "a3"
    case (A4, x) => "a4"
    case (x, A4) => "a4"
    case (A5, x) => "a5"
    case (x, A5) => "a5"
    case (A6, x) => "a6"
    case (x, A6) => "a6"
    case (a, b)  => "ab"
  }
}
