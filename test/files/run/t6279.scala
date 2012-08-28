


import collection._



object Test {

  def main(args: Array[String]) {
    val ppc: List[Int] = List(1, 2, 3).par ++: List(4, 5, 6)
    assert(ppc == List(1, 2, 3, 4, 5, 6), ppc)

    {
      import JavaConversions._
      val arraylistwrapper: mutable.Buffer[Int] = new java.util.ArrayList[Int]
      arraylistwrapper += 0
      arraylistwrapper += 4
      arraylistwrapper.insertAll(1, List(1, 2, 3).par)
      assert(arraylistwrapper == Seq(0, 1, 2, 3, 4), arraylistwrapper)
    }

    val concatlist: List[Int] = List.concat(List(1, 2, 3), Array(4, 5, 6).par)
    assert(concatlist == List(1, 2, 3, 4, 5, 6), concatlist)

    val ppe = new mutable.ArrayBuffer() ++= Seq(1, 2, 3, 4, 5, 6).par
    assert(ppe == Seq(1, 2, 3, 4, 5, 6), ppe)

    val mme = mutable.Set(1, 2, 3, 4, 5, 6) --= Set(4, 5, 6).par
    assert(mme == Set(1, 2, 3), mme)

    val ppec: mutable.ArrayBuffer[Int] = List(-4, -3, -2, -1, 0) ++=: mutable.ArrayBuffer(1, 2, 3, 4)
    assert(ppec == Seq(-4, -3, -2, -1, 0, 1, 2, 3, 4), ppec)

    val ia = mutable.ArrayBuffer(1, 6)
    ia.insertAll(1, Array(2, 3, 4, 5).par)
    assert(ia == Seq(1, 2, 3, 4, 5, 6), ia)

    val ab = new mutable.ArrayBuilder.ofRef[String]
    ab ++= Array("_", "?", "!")
    ab ++= Array("a", "ab", "b", "bc", "cd", "ce").par
    assert(ab.result.toList == Seq("_", "?", "!", "a", "ab", "b", "bc", "cd", "ce"), ab)

    val fl = Array((-100 until 2).par, Seq(2, 3, 4), List(5, 6, 7, 8, 9).par)
    val flattened = fl.flatten
    assert(flattened.toList == (-100 until 10), flattened)

    val pa = mutable.ArrayBuffer(1, 2, 3, 4, 5, 6)
    pa.prependAll((-100 until 1).par)
    assert(pa == (-100 until 7), pa)

    val aa = mutable.ArrayBuffer(1, 2, 3, 4, 5, 6)
    aa.appendAll((7 until 100).par)
    assert(aa == (1 until 100), aa)

    val stack = mutable.Stack(6, 5, 4, 3, 2, 1)
    stack.pushAll((7 to 100).par)
    assert(stack == (100 until 0 by -1), stack)

    val listbuffer = mutable.ListBuffer[Int](4, 5, 6)
    listbuffer ++= (7 until 100).par
    (-100 until 4).par ++=: listbuffer
    assert(listbuffer == (-100 until 100), listbuffer)
  }

}