// $Id$

import scala.util.continuations._


object Test {

  class Bla {
    val x = 8
    def y[T] = 9
  }

/*
  def bla[A] = shift { k:(Bla=>A) => k(new Bla) }
*/

  def bla1 = shift { k:(Bla=>Bla) => k(new Bla) }
  def bla2 = shift { k:(Bla=>Int) => k(new Bla) }

  def fooA = bla2.x
  def fooB[T] = bla2.y[T]

  def testMono() = {
    println(reset(bla1).x)
    println(reset(bla2.x))
    println(reset(bla2.y[Int]))
    println(reset(bla2.y))
    println(reset(fooA))
    println(reset(fooB))
    0
  }

  def blaX[A] = shift { k:(Bla=>A) => k(new Bla) }
  
  def fooX[A] = blaX[A].x
  def fooY[A] = blaX[A].y[A]
  
  def testPoly() = {
    println(reset(blaX[Bla]).x)
    println(reset(blaX[Int].x))
    println(reset(blaX[Int].y[Int]))
    println(reset(blaX[Int].y))
    println(reset(fooX[Int]))
    println(reset(fooY[Int]))
    0
  }


  // TODO: check whether this also applies to a::shift { k => ... }

  def main(args: Array[String]) = {
    testMono()
    testPoly()
  }
  
}
