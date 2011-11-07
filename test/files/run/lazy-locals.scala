
object Test extends App {

  lazy val w = 10

  /** normal test */
  def testLazy = {
    lazy val t = { Console.println("forced lazy val t"); 42 }
    lazy val p = t / 2
    lazy val q = { println("forced lazy val q"); 10}
    println("q = " + q)
    println("p = " + p)
    1 + t + t
  }

  /** test 32 lazy vals, which fills one bitmap int. */
  def testLazy32 = {
    lazy val t00 = { Console.println("forced lazy val t00");  0 }
    lazy val t01 = { Console.println("forced lazy val t01");  1 }
    lazy val t02 = { Console.println("forced lazy val t02");  2 }
    lazy val t03 = { Console.println("forced lazy val t03");  3 }
    lazy val t04 = { Console.println("forced lazy val t04");  4 }
    lazy val t05 = { Console.println("forced lazy val t05");  5 }
    lazy val t06 = { Console.println("forced lazy val t06");  6 }
    lazy val t07 = { Console.println("forced lazy val t07");  7 }
    lazy val t08 = { Console.println("forced lazy val t08");  8 }
    lazy val t09 = { Console.println("forced lazy val t09");  9 }
    lazy val t10 = { Console.println("forced lazy val t10"); 10 }
    lazy val t11 = { Console.println("forced lazy val t11"); 11 }
    lazy val t12 = { Console.println("forced lazy val t12"); 12 }
    lazy val t13 = { Console.println("forced lazy val t13"); 13 }
    lazy val t14 = { Console.println("forced lazy val t14"); 14 }
    lazy val t15 = { Console.println("forced lazy val t15"); 15 }
    lazy val t16 = { Console.println("forced lazy val t16"); 16 }
    lazy val t17 = { Console.println("forced lazy val t17"); 17 }
    lazy val t18 = { Console.println("forced lazy val t18"); 18 }
    lazy val t19 = { Console.println("forced lazy val t19"); 19 }
    lazy val t20 = { Console.println("forced lazy val t20"); 20 }
    lazy val t21 = { Console.println("forced lazy val t21"); 21 }
    lazy val t22 = { Console.println("forced lazy val t22"); 22 }
    lazy val t23 = { Console.println("forced lazy val t23"); 23 }
    lazy val t24 = { Console.println("forced lazy val t24"); 24 }
    lazy val t25 = { Console.println("forced lazy val t25"); 25 }
    lazy val t26 = { Console.println("forced lazy val t26"); 26 }
    lazy val t27 = { Console.println("forced lazy val t27"); 27 }
    lazy val t28 = { Console.println("forced lazy val t28"); 28 }
    lazy val t29 = { Console.println("forced lazy val t29"); 29 }
    lazy val t30 = { Console.println("forced lazy val t30"); 30 }
    lazy val t31 = { Console.println("forced lazy val t31"); 31 }

    val sum = t31 + t30 + t29 + t28 + t27 + t26 + t25 + t24 + t23 +
              t22 + t21 + t20 + t19 + t18 + t17 + t16 + t15 + t14 +
              t13 + t12 + t11 + t10 + t09 + t08 + t07 + t06 + t05 +
              t04 + t03 + t02 + t01 + t00;
    println("Sum is: " + sum);
  }

  /** test 32 lazy vals, which needs two bitmap ints. */
  def testLazy33 = {
    lazy val t00 = { Console.println("forced lazy val t00");  0 }
    lazy val t01 = { Console.println("forced lazy val t01");  1 }
    lazy val t02 = { Console.println("forced lazy val t02");  2 }
    lazy val t03 = { Console.println("forced lazy val t03");  3 }
    lazy val t04 = { Console.println("forced lazy val t04");  4 }
    lazy val t05 = { Console.println("forced lazy val t05");  5 }
    lazy val t06 = { Console.println("forced lazy val t06");  6 }
    lazy val t07 = { Console.println("forced lazy val t07");  7 }
    lazy val t08 = { Console.println("forced lazy val t08");  8 }
    lazy val t09 = { Console.println("forced lazy val t09");  9 }
    lazy val t10 = { Console.println("forced lazy val t10"); 10 }
    lazy val t11 = { Console.println("forced lazy val t11"); 11 }
    lazy val t12 = { Console.println("forced lazy val t12"); 12 }
    lazy val t13 = { Console.println("forced lazy val t13"); 13 }
    lazy val t14 = { Console.println("forced lazy val t14"); 14 }
    lazy val t15 = { Console.println("forced lazy val t15"); 15 }
    lazy val t16 = { Console.println("forced lazy val t16"); 16 }
    lazy val t17 = { Console.println("forced lazy val t17"); 17 }
    lazy val t18 = { Console.println("forced lazy val t18"); 18 }
    lazy val t19 = { Console.println("forced lazy val t19"); 19 }
    lazy val t20 = { Console.println("forced lazy val t20"); 20 }
    lazy val t21 = { Console.println("forced lazy val t21"); 21 }
    lazy val t22 = { Console.println("forced lazy val t22"); 22 }
    lazy val t23 = { Console.println("forced lazy val t23"); 23 }
    lazy val t24 = { Console.println("forced lazy val t24"); 24 }
    lazy val t25 = { Console.println("forced lazy val t25"); 25 }
    lazy val t26 = { Console.println("forced lazy val t26"); 26 }
    lazy val t27 = { Console.println("forced lazy val t27"); 27 }
    lazy val t28 = { Console.println("forced lazy val t28"); 28 }
    lazy val t29 = { Console.println("forced lazy val t29"); 29 }
    lazy val t30 = { Console.println("forced lazy val t30"); 30 }
    lazy val t31 = { Console.println("forced lazy val t31"); 31 }
    lazy val t32 = { Console.println("forced lazy val t32"); 32 }

    val sum = t32 + t31 + t30 + t29 + t28 + t27 + t26 + t25 + t24 + t23 +
              t22 + t21 + t20 + t19 + t18 + t17 + t16 + t15 + t14 +
              t13 + t12 + t11 + t10 + t09 + t08 + t07 + t06 + t05 +
              t04 + t03 + t02 + t01 + t00;
    println("Sum is: " + sum);
  }


  /** test recursive method with lazy vals and a single forced */
  def testLazyRec(n: Int): Int = {
    lazy val t = { println("forced lazy val t at n = " + n); 42 }
    if (n > 0)
      testLazyRec(n - 1)
    else
      t
  }

  /** test recursive method with lazy vals and a all vals forced */
  def testLazyRecMany(n: Int): Int = {
    lazy val t = { println("forced lazy val t at n = " + n); 42 }
    if (n > 0) {
      testLazyRecMany(n - 1);
      t*t
    } else
      t
  }

  def testRecVal {
    lazy val twos: List[Int] = 2 :: twos
    lazy val ones: Stream[Int] = Stream.cons(1, ones)

    println("First 5 elements of ones: " + ones.take(5).toList)
  }

  // should compile without error
  def testMutualRecVal {
    lazy val odd: Int = 1 + even
    lazy val even: Int = 1 + odd

    ()
  }

  def testReturnInLazyVal: Boolean = {
    lazy val go = { return false }
    go
  }

  {
    lazy val inCtor = "I am initialized when the constructor is run"
    inCtor
  }

  class CtorBlock {
    {
      lazy val inCtor = {
        println("I am initialized when the constructor is run")
        42
      }
      inCtor
    }
  }

  // ticket #1589, should not crash
  class Test {
    val x = {
      lazy val t = "abc";
      t
    }
  }

  // see #1589
  object NestedLazyVals {
    lazy val x = { 
      lazy val y = { println("forcing y"); 42; }
      println("forcing x")
      y 
    }
    
    val x1 = 5 + { lazy val y = 10 ; y }
    
    println(x)
    println(x1)
  }
  
  trait TNestedLazyVals {
    lazy val x = { lazy val y = 42; y }
  }

  object ONestedLazyVals extends TNestedLazyVals {
    println(x)
  }

  println(testLazy)
  testLazy32
  testLazy33
  println(testLazyRec(5))
  println(testLazyRecMany(5))
  testRecVal
  new CtorBlock
  println(testReturnInLazyVal)
  NestedLazyVals
  ONestedLazyVals
}
