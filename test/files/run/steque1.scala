// testing the impl from the scala library
//package test5

import scala.collection._
import scala.collection.immutable._

import scala.collection.generic._
import scala.collection.mutable.Builder


object Test {

    def steque(label: String, n: Int): Steque[String] = {
      val a = Steque.newBuilder[String]
      for (i <- 0 until n)
        a += (label + i)

      val res = a.result
      assertSteque(res, label, 0, n)
    }

    def stequeForward(label: String, n: Int): Steque[String] = {
      var a = Steque.empty[String]
      for (i <- 0 until n)
        a = a :+ (label + i)

      assertSteque(a, label, 0, n)
    }

    def stequeBackward(label: String, n: Int): Steque[String] = {
      var a: Steque[String] = Steque.empty
      for (i <- 0 until n)
        a = (label + (n-1-i)) +: a

      assertSteque(a, label, 0, n)
    }

    def assertSteque[V](a: Steque[V], label: String, start: Int, end: Int) = {
      assertStequeIndexed(a, label, start, end)
      assertStequeIterated(a, label, start, end)
    }

    def assertStequeIndexed[V](a: Steque[V], label: String, start: Int, end: Int) = {
      val res = a
      assert(res.length == (end-start), res.length+"!="+(end-start)+" ("+res+")")
      for (i <- start until end) {
        assert(res(i) == (label + i), ""+res(i)+"!="+(label + i))
      }
      res
    }

    def assertStequeIterated[V](a: Steque[V], label: String, start: Int, end: Int) = {
      val res = a
      assert(res.length == (end-start), res.length+"!="+(end-start)+" ("+res+")")
      var i = start
      var it = res.iterator
      while(it.hasNext) {
        val x = it.next()
        assert(x == (label + i), x.toString+"!="+(label + i))
        i += 1
      }
      assert(i == end)
      res
    }



  def test1() = {
    println("===== test1 =====")

    val N = 200
    val a = steque("a", N)
    val b = stequeForward("b", N)
    val c = stequeBackward("b", N)

    ()
//    //println(a)
  }

  def test2() = {
    println("===== test2 =====")

    var a: Steque[String] = Steque.empty

    val rand = new java.util.Random

    val N = 200
    var min = N/2//rand.nextInt(N)
    var max = min

    val chunkLimit = 11

    def nextChunkSize = 3 //rand.nextInt(chunkLimit)

    def seqBack() = for (i <- 0 until Math.min(nextChunkSize, N-max)) { a = a :+ ("a"+max); max += 1 }
    def seqFront() = for (i <- 0 until Math.min(nextChunkSize, min)) { min -= 1; a = ("a"+min) +: a }

    try {

    while (min > 0 || max < N) {
      seqFront()
      seqBack()
    }
  } catch {
    case ex: Throwable =>
      //println("----------------")
      //a.debug
      throw ex
  }

    assertSteque(a, "a", 0, N)
  }



  def test3() = {
    println("===== test3 =====")

    val N = 200
    val a = steque("a", N)

    val pos = scala.util.Random.shuffle(scala.collection.mutable.WrappedArray.make[Int](Array.tabulate[Int](N)(i => i)))

    var b = a

    {
      var i = 0
      while (i < N) {
        b = b.updated(pos(i), "b"+(pos(i)))
        i += 1
      }

      assertSteque(b, "b", 0, N)
    }

//    //println(a)
  }

  def test4() = {
    println("===== test4 =====")

    val N = 200
    val a = stequeForward("a", N)

    {
      var i = 0
      var it = a
      while (i < N) {
        assert(it.length == (N-i), it.length+" items at iteration "+i)
        val x = it(0)
        val y = it(N-i-1)
        assert(x == "a"+i, x+"!=a"+i)
        assert(y == "a"+(N-1), y+"!=a"+(N-1))
        it = it.drop(1)
        i += 1
      }
      assert(it.length == 0)
    }

//    //println(a)
  }

  def test5() = {
    println("===== test5 =====")

    val N = 200
    val a = stequeBackward("a", N)

    {
      var i = 0
      var it = a
      while (i < N) {
        assert(it.length == (N-i), it.length+" items at iteration "+i)
        val x = it(0)
        val y = it(N-i-1)
//        println("x " + x + "/" + i)
//        println("y " + y)
        assert(x == "a0", x+"!=a0")
        assert(y == "a"+(N-i-1), y+"!=a"+(N-i-1))
        it = it.dropRight(1)
        i += 1
      }
      assert(it.length == 0)
    }
  }

  def main(args: Array[String]) = {

    test1()
    test2()
    test3()
    test4()
    test5()

    println("done")
  }

}

