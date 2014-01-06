import scala.collection.mutable.TreeSet

object Test extends App {
  val list = List(6,5,4,3,2,1,1,2,3,4,5,6,6,5,4,3,2,1)
  val distinct = list.distinct
  val sorted = distinct.sorted

  // sublist stuff for a single level of slicing
  val min = list.min
  val max = list.max
  val nonlist = ((min - 10) until (max + 20) filterNot list.contains).toList
  val sublist = list filter {x => x >=(min + 1) && x < max}
  val distinctSublist = sublist.distinct
  val subnonlist = min :: max :: nonlist
  val subsorted = distinctSublist.sorted

  // subsublist for a 2nd level of slicing
  val almostmin = sublist.min
  val almostmax = sublist.max
  val subsublist = sublist filter {x => x >=(almostmin + 1) && x < almostmax}
  val distinctSubsublist = subsublist.distinct
  val subsubnonlist = almostmin :: almostmax :: subnonlist
  val subsubsorted = distinctSubsublist.sorted

  def testSize {
    def check(set : TreeSet[Int], list: List[Int]) {
      assert(set.size == list.size, s"$set had size ${set.size} while $list had size ${list.size}")
    }

    check(TreeSet[Int](), List[Int]())
    val set = TreeSet(list:_*)
    check(set, distinct)
    check(set.clone, distinct)

    val subset = set from (min + 1) until max
    check(subset, distinctSublist)
    check(subset.clone, distinctSublist)

    val subsubset = subset from (almostmin + 1) until almostmax
    check(subsubset, distinctSubsublist)
    check(subsubset.clone, distinctSubsublist)
  }

  def testContains {
    def check(set : TreeSet[Int], list: List[Int], nonlist: List[Int]) {
      assert(list forall set.apply, s"$set did not contain all elements of $list using apply")
      assert(list forall set.contains, s"$set did not contain all elements of $list using contains")
      assert(!(nonlist exists set.apply), s"$set had an element from $nonlist using apply")
      assert(!(nonlist exists set.contains), s"$set had an element from $nonlist using contains")
    }

    val set = TreeSet(list:_*)
    check(set, list, nonlist)
    check(set.clone, list, nonlist)

    val subset = set from (min + 1) until max
    check(subset, sublist, subnonlist)
    check(subset.clone, sublist, subnonlist)

    val subsubset = subset from (almostmin + 1) until almostmax
    check(subsubset, subsublist, subsubnonlist)
    check(subsubset.clone, subsublist, subsubnonlist)
  }

  def testAdd {
    def check(set : TreeSet[Int], list: List[Int], nonlist: List[Int]) {
      var builtList = List[Int]()
      for (x <- list) {
        set += x
        builtList = (builtList :+ x).distinct.sorted filterNot nonlist.contains
        assert(builtList forall set.apply, s"$set did not contain all elements of $builtList using apply")
        assert(builtList.size == set.size, s"$set had size ${set.size} while $builtList had size ${builtList.size}")
      }
      assert(!(nonlist exists set.apply), s"$set had an element from $nonlist using apply")
      assert(!(nonlist exists set.contains), s"$set had an element from $nonlist using contains")
    }

    val set = TreeSet[Int]()
    val clone = set.clone
    val subset = set.clone from (min + 1) until max
    val subclone = subset.clone
    val subsubset = subset.clone from (almostmin + 1) until almostmax
    val subsubclone = subsubset.clone

    check(set, list, nonlist)
    check(clone, list, nonlist)

    check(subset, list, subnonlist)
    check(subclone, list, subnonlist)

    check(subsubset, list, subsubnonlist)
    check(subsubclone, list, subsubnonlist)
  }

  def testRemove {
    def check(set: TreeSet[Int], sorted: List[Int]) {
      var builtList = sorted
      for (x <- list) {
        set remove x
        builtList = builtList filterNot (_ == x)
        assert(builtList forall set.apply, s"$set did not contain all elements of $builtList using apply")
        assert(builtList.size == set.size, s"$set had size $set.size while $builtList had size $builtList.size")
      }
    }
    val set = TreeSet(list:_*)
    val clone = set.clone
    val subset = set.clone from (min + 1) until max
    val subclone = subset.clone
    val subsubset = subset.clone from (almostmin + 1) until almostmax
    val subsubclone = subsubset.clone

    check(set, sorted)
    check(clone, sorted)

    check(subset, subsorted)
    check(subclone, subsorted)

    check(subsubset, subsubsorted)
    check(subsubclone, subsubsorted)
  }

  def testIterator {
    def check(set: TreeSet[Int], list: List[Int]) {
      val it = set.iterator.toList
      assert(it == list, s"$it did not equal $list")
    }
    val set = TreeSet(list: _*)
    check(set, sorted)
    check(set.clone, sorted)

    val subset = set from (min + 1) until max
    check(subset, subsorted)
    check(subset.clone, subsorted)

    val subsubset = subset from (almostmin + 1) until almostmax
    check(subsubset, subsubsorted)
    check(subsubset.clone, subsubsorted)
  }

  testSize
  testContains
  testAdd
  testRemove
  testIterator
}
