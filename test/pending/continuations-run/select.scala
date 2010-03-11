// $Id$

import scala.util.continuations._


object Test {

  class Bla {
    val x = 8
  }

  def bla = shift { k:(Bla=>Bla) => k(new Bla) }

  // TODO: check whether this also applies to a::shift { k => ... }

  def main(args: Array[String]) = {
    println(reset(bla).x)
    println(reset(bla.x))
  }

}
