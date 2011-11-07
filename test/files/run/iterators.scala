//############################################################################
// Iterators
//############################################################################

//############################################################################

object Test {

  def check_from: Int = {
    val it1 = Iterator.from(-1)
    val it2 = Iterator.from(0, -1)
    it1.next + it2.next
  }

  def check_range: Int = {
    val xs1 = Iterator.range(0, 10,  2) toList;
    val xs2 = Iterator.range(0, 10, -2) toList;
    val xs3 = Iterator.range(10, 0, -2) toList;
    val xs4 = Iterator.range(10, 0,  2) toList;
    val xs5 = Iterator.range(0, 10, 11) toList;
    xs1.length + xs2.length + xs3.length + xs4.length + xs5.length
  }

  def check_range2: Int = {
    val r1start = 0
    val r1end = 10
    val r1step = 1
    val r1 = Iterator.range(r1start, r1end, r1step) toList;
    val r2 = Iterator.range(r1start, r1end, r1step + 1) toList;
    val r3 = Iterator.range(r1end, r1start, -r1step) toList;
    val r4 = Iterator.range(0, 10, 11) toList;
    // 10 + 5 + 10 + 1
    r1.length + r2.length + r3.length + r4.length
  }

  def check_range3: Int = {
    def trues(xs: List[Boolean]) = xs.foldLeft(0)((a, b) => if (b) a+1 else a)
    val r1 = Iterator.range(0, 10)
    val xs1 = List(r1 contains 5, r1 contains 6)
    val r2a = Iterator.range(0, 10, 2)
    val r2b = Iterator.range(0, 10, 2)
    val xs2 = List(r2a contains 5, r2b contains 6)
    val r3 = Iterator.range(0, 10, 11)
    val xs3 = List(r3 contains 5, r3 contains 6)
    // 2 + 1 + 0
    trues(xs1) + trues(xs2) + trues(xs3)
  }

  def check_take: Int = {
    val it1 = Iterator.from(0)
    val xs1 = it1 take 10 toList;
    xs1.length
  }

  def check_drop: Int = {
    val it1 = Iterator.from(0)
    val it2 = it1 map { 2 * _ }
    val n1 = it1 drop 2 next
    val n2 = it2 drop 2 next;
    n1 + n2
  }

  def check_foreach: Int = {
    val it1 = Iterator.from(0) take 20
    var n = 0
    it1 foreach { n += _ }
    n
  }

  def check_forall: Int = {
    val it1 = Iterator.from(0)
    val it2 = Iterator.from(1)
    0
  }

  def check_fromArray: Int = {  // ticket #429
    val a = List(1, 2, 3, 4).toArray
    var xs0 = Iterator.fromArray(a).toList;
    var xs1 = Iterator.fromArray(a, 0, 1).toList;
    var xs2 = Iterator.fromArray(a, 0, 2).toList;
    var xs3 = Iterator.fromArray(a, 0, 3).toList;
    var xs4 = Iterator.fromArray(a, 0, 4).toList;
    xs0.length + xs1.length + xs2.length + xs3.length + xs4.length
  }
  
  def check_toSeq: String =
    List(1, 2, 3, 4, 5).iterator.toSeq.mkString("x")
  
  def check_indexOf: String = {
    val i = List(1, 2, 3, 4, 5).indexOf(4)
    val j = List(1, 2, 3, 4, 5).indexOf(16)
    "" + i + "x" + j
  }
  
  def check_findIndexOf: String = {
    val i = List(1, 2, 3, 4, 5).indexWhere { x: Int => x >= 4 }
    val j = List(1, 2, 3, 4, 5).indexWhere { x: Int => x >= 16 }
    "" + i + "x" + j
  }

  def check_success[A](name: String, closure: => A, expected: A) {
    print("test " + name)
    try {
      val actual: A = closure
      if (actual == expected)
        print(" was successful")
      else
        print(" failed: expected "+ expected +", found "+ actual)
    }
    catch {
      case exception: Throwable =>
        print(" raised exception " + exception)
    }
    println()
  }

  def main(args: Array[String]) {
    check_success("check_from",     check_from,      -1)
    check_success("check_range",    check_range,     11)
    check_success("check_range2",   check_range2,    26)
    check_success("check_range3",   check_range3,     3)
    check_success("check_take",     check_take,      10)
    check_success("check_drop",     check_drop,      12)
    check_success("check_foreach",  check_foreach,  190)
    check_success("check_forall",   check_forall,     0)
    check_success("check_fromArray",check_fromArray, 14)
    check_success("check_toSeq",  check_toSeq, "1x2x3x4x5")
    check_success("check_indexOf",     check_indexOf,     "3x-1")
    check_success("check_findIndexOf", check_findIndexOf, "3x-1")
    println()
  }
}

//############################################################################
