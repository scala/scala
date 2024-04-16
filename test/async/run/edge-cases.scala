//> using options -Xasync

import scala.tools.partest.async.OptionAwait._
import org.junit.Assert._

// Scala.js compatible test suite for -Xasync that doesn't use Scala futures
object Test {
  def main(args: Array[String]): Unit = {
    patternTailPosition()
    mixedBagNPE()
    patternTailPositionMatchEndCast()
    awaitTyped()
    avoidLiftingTryIntoExpression()
    avoidLiftingTryIntoExpression2()
    avoidWhileExprPosition()
    whileExpr1()
    genericUnitTypedAssert()
    unitTypedValAwaitingWhileRhs()
    nestedBlock()
    matchWithIf()
    testMatchBig()
    testLiftedLazyVal()
    testWhile1()
    testWhile2()
    testCaseClassLifting()
    testNothingTypedExpr()
    testLambdaLiftClash()
    testWhileExprInIf()
  }

  private def patternTailPosition() = {
    assertEquals(Some("foo"), optionally {
      {
        value(Some(1))
        "foo" match {
          case x if "".isEmpty => x
          case x               => throw new MatchError(x)
        }
      }: AnyRef
    })
  }

  def mixedBagNPE(): Unit = {
    class A {
      def f = Some(this)
    }
    val data = List(("0", "0"))

    assertEquals(Some(()), optionally {
      val s1 = value(new A().f)
      s1.toString
      val s2 = value(s1.f)
      s2.toString
      val it = data.iterator
      while (it.hasNext) {
        val v = it.next()
        v match {
          case (x, y) =>
            "".isEmpty
            val r1 = value(s2.f).toString
            val r2 = value(s2.f).toString
            (r1, r2)
            val it = List("").iterator
            while (it.hasNext) {
              val v = it.next()
              val r = value(s2.f).equals(v)
            }
        }
      }
    })
  }

  def patternTailPositionMatchEndCast(): Unit = {
    trait A1
    trait B1
    trait B2
    trait Z1
    class C1 extends B1 with A1 with Z1
    class C2 extends A1 with B2 with Z1
    class C2A extends C2
    class C2B extends C2
    assertEquals(Some(null), optionally[Z1] {
      val result = if ("foo".isEmpty) {
        value(Option(1))
        null: Z1
      } else {
        (0: Any) match {
          case 0 =>
            value(Option(1))
            null: C2 with Z1
          case _ =>
            null: C1 with Z1
        }
      }
      result
    })
  }

  def awaitTyped(): Unit = {
    assertEquals(Some("msg: 0"), optionally {
      (("msg: " + value(Option(0))): String).toString
    })
  }

  def avoidLiftingTryIntoExpression(): Unit = {
    assertEquals(Some(("body1", "body2")), optionally {
      //var info = ""
      var info = try {
        "body1"
      } catch {
        case _: Throwable => "fallback"
      }
      var info1 = ""
      info1 = try {
        "body2"
      } catch {
        case _: Throwable => "fallback"
      }
      value(Option(0))
      (info, info1)
    })
  }

  def avoidLiftingTryIntoExpression2(): Unit = {
    assertEquals(Some("body"), optionally {
      value(Some(1))
      try {
        "body"
      } catch {
        case _: Throwable =>
          "catch"
      }
    })
  }
  def avoidWhileExprPosition(): Unit = {
    assertEquals(Some("result"), optionally {
      if ("".isEmpty) {
        ()
      } else {
        var continue = true
        while (continue) {
          continue = value(Option(false))
        }
      }
      value(Option("result"))
    })
  }
  def whileExpr1(): Unit = {
    var continue = true

    def test = optionally {
      (value(test1), value(test2))
    }

    def test1 = optionally {
      while (continue) {
        continue = false
        value(Option(()))
      }
      value(Option("result1"))
    }

    def test2 = optionally {
      value(Option(""))
      while (continue) {
        continue = false
        value(Option(()))
      }
      value(Option("result2"))
    }

    assertEquals(Some(("result1", "result2")), optionally {
      (value(test1), value(test2))
    })
  }

  def genericUnitTypedAssert(): Unit = {
    def finish[T](t: T): T = t

    assertEquals(Some(()), optionally {
      finish(this match { case _ if "".isEmpty => (); case _ => value(Option(())) })
    })
  }

  def unitTypedValAwaitingWhileRhs(): Unit = {
    def finish[T](t: T): T = t

    var continue = true
    assertEquals(Some("result"), optionally {
      val x = while (continue) {
        value(Option(()))
        continue = false
        ()
      }
      "result"
    })
  }

  def nestedBlock(): Unit = {
    def condition = true

    assertEquals(Some(()), optionally {
      if (condition) {
        toString
        if (condition) {
          scala.runtime.BoxedUnit.UNIT
        } else {
          ({
            if (condition) value(Option("")); scala.runtime.BoxedUnit.UNIT
          }: scala.runtime.BoxedUnit)
        }
      } else {
        identity(())
      }
    })
  }

  def matchWithIf(): Unit = {
    def finish[T](t: T): T = t

    def condition = true

    def scrut: Some[AnyRef] = Some("")

    assertEquals(Some(("a", "then")), optionally {
      scrut match {
        case Some(_) =>
          val a = "a"
          val x = if (condition) "then" else {
            value(Option("")); "else"
          }
          identity((a, x))
      }
    })
  }

  def testMatchBig(): Unit = {
    assertEquals(Some(5), optionally {
      val x: Option[Either[Object, (String, String)]] = Some(Right(("a", "b")))
      x match {
        case Some(Left(_)) => 1
        case Some(Right(("a", "c"))) => 2
        case Some(Right(("a", "e"))) => 3
        case Some(Right(("a", x))) if "ab".isEmpty => 4
        case Some(Right(("a", "b"))) => value(Option(5))
        case Some(Right((y, x))) if x == y => 6
        case Some(Right((_, _))) => value(Option(7))
        case None => 8
      }
    })
  }

  def testLiftedLazyVal(): Unit = {
    assertEquals(Some((1, 1, -1)), optionally {
      var i = 0;
      var z = 0
      lazy val foo = {
        def ii = i; z = -1; ii
      };
      value(Option(1)) + value(Option(2));
      i += 1;
      (foo, foo, z)
    })
  }

  def testWhile1(): Unit = {
    assertEquals(Some(100), optionally {
      var sum = 0
      var i = 0
      while (i < 5) {
        var j = 0
        while (j < 5) {
          sum += value(Some(i)) * value(Some(j))
          j += 1
        }
        i += 1
      }
      sum
    })
  }

  def testWhile2(): Unit = {
    assertEquals(Some(10), optionally {
      var sum = 0
      var i = 0
      while (i < 5) {
        sum += value(Option(i))
        i += 1
      }
      sum
    })
  }

  def testCaseClassLifting(): Unit = {
    assertEquals(Some("bob"), optionally {
      trait Base { def base = 0}
      value(Option(0))
      case class Person(name: String) extends Base
      val fut = Option("bob")
      val x = Person(value(fut))
      x.base
      assert(Person.getClass.getName == classOf[Person].getName + "$", (Person.getClass.getName, classOf[Person].getName))
      x.name
    })
  }

  def testNothingTypedExpr(): Unit = {
    try {
      optionally { if ("".isEmpty) {value(Option("")); throw new RuntimeException("boo!")} else ??? }
      assert(false)
    } catch {
      case re: RuntimeException => assert(re.getMessage == "boo!")
      case _: Throwable => assert(false)
    }
  }

  // If async and lambdalift phase both use the compilation units FreshNameCreator, we get foo$1 and foo$2, no clash!
  def testLambdaLiftClash(): Unit = {
   assertEquals(Some(42), optionally {
     def foo = 42
     value(Option("")); // so that the preceding def will be lifted to foo$N

     {
       // lambdalift will later lift this to foo$N.
       def foo = 43
       foo
     };
     foo
   })
  }

  def testWhileExprInIf(): Unit = {
    val t: Any = optionally {
      if ("".isEmpty) {
        ()
      } else {
        val it = Nil.iterator
        while (it.hasNext) {
          value(Some(it.next()))
        }
      }
    }.get
  }
}
