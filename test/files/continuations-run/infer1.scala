// $Id$

import scala.util.continuations._


object Test {
 
  def test(x: => Int @cpsParam[String,Int]) = 7
  
  def test2() = {
    val x = shift { k: (Int => String) => 9 }
    x
  }

  def test3(x: => Int @cpsParam[Int,Int]) = 7

  
  def util() = shift { k: (String => String) => "7" }
  
  def main(args: Array[String]): Any = {
    test { shift { k: (Int => String) => 9 } }
    test { shift { k: (Int => String) => 9 }; 2 }
//    test { shift { k: (Int => String) => 9 }; util() }  <-- doesn't work
    test { shift { k: (Int => String) => 9 }; util(); 2 }


    test { shift { k: (Int => String) => 9 }; { test3(0); 2 } }

    test3 { { test3(0); 2 } }

  }
  
}