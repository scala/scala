package scala.collection.parallel.ops


import scala.collection.parallel._


trait IntOperators extends Operators[Int] {
  def reduceOperators = List(_ + _, _ * _, math.min(_, _), math.max(_, _), _ ^ _)
  def countPredicates = List(
    x => true,
    _ >= 0, _ < 0, _ < 50, _ < 500, _ < 5000, _ < 50000, _ % 2 == 0, _ == 99,
    x => x > 50 && x < 150,
    x => x > 350 && x < 550,
    x => (x > 1000 && x < 1500) || (x > 400 && x < 500)
  )
  def forallPredicates = List(_ >= 0, _ < 0, _ % 2 == 0, _ != 55, _ != 505, _ != 5005)
  def existsPredicates = List(_ >= 0, _ < 0, _ % 2 == 0, _ == 55, _ == 505, _ == 5005)
  def findPredicates = List(_ >= 0, _ % 2 == 0, _ < 0, _ == 50, _ == 500, _ == 5000)
  def mapFunctions = List(-_, math.abs(_), _ % 2, _ % 3, _ % 4, _ % 150, _ % 500)
  def partialMapFunctions = List({case x => -x}, { case 0 => -1; case x if x > 0 => x + 1}, {case x if x % 3 == 0 => x / 3})
  def flatMapFunctions = List(
    (n: Int) => if (n < 0) List() else if (n % 2 == 0) List(1, 2, 3) else List(4, 5, 6),
    (n: Int) => List[Int](),
    (n: Int) => if (n == 0) List(1, 2, 3, 4, 5) else if (n < 0) List(1, 2, 3) else List()
  )
  def filterPredicates = List(
    _ % 2 == 0, _ % 3 == 0,
    _ % 4 != 0, _ % 17 != 0,
    n => n > 50 && n < 100,
    _ >= 0, _ < 0, _ == 99,
    _ > 500, _ > 5000, _ > 50000,
    _ < 500, _ < 50, _ < -50, _ < -5e5,
    x => true, x => false,
    x => x % 53 == 0 && x % 17 == 0
  )
  def filterNotPredicates = filterPredicates
  def partitionPredicates = filterPredicates
  def takeWhilePredicates = List(
    _ != 50, _ != 500, _ != 5000, _ != 50000, _ % 2 == 0, _ % 3 == 1, _ % 47 != 0,
    _ < 100, _ < 1000, _ < 10000, _ < 0,
    _ < -100, _ < -1000, _ > -200, _ > -50,
    n => -90 < n && n < -10,
    n => 50 < n && n < 550,
    n => 5000 < n && n < 7500,
    n => -50 < n && n < 450
  )
  def dropWhilePredicates = takeWhilePredicates
  def spanPredicates = takeWhilePredicates
  def foldArguments = List(
    (0, _ + _),
    (1, _ * _),
    (Int.MinValue, math.max(_, _)),
    (Int.MaxValue, math.min(_, _))
  )
  def addAllTraversables = List(
    List[Int](),
    List(1),
    List(1, 2),
    List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    Array.fill(1000)(1).toSeq
  )
  def newArray(sz: Int) = new Array[Int](sz)
  def groupByFunctions = List(
    _ % 2, _ % 3, _ % 5, _ % 10, _ % 154, _% 3217,
    _ * 2, _ + 1
  )
}


trait IntSeqOperators extends IntOperators with SeqOperators[Int] {
  def segmentLengthPredicates = List(
    _ % 2 == 0, _ > 0, _ >= 0, _ < 0, _ <= 0, _ > -5000, _ > 5000, _ % 541 != 0, _ < -50, _ > 500,
    n => -90 < n && n < -10, n => 500 < n && n < 1500
  )
  def indexWherePredicates = List(
    _ % 2 == 0, _ % 11 == 0, _ % 123 == 0, _ % 901 == 0,
    _ > 0, _ >= 0, _ < 0, _ <= 0,
    _ > 50, _ > 500, _ > 5000,
    _ < -10, _ < -100, _ < -1000,
    n => n > 50 && n < 100,
    n => n * n > 1000000 && n % 111 == 0
  )
  def lastIndexWherePredicates = List(
    _ % 2 == 0, _ % 17 == 0, _ % 314 == 0, _ % 1017 == 0,
    _ > 0, _ >= 0, _ < 0, _ <= 0,
    _ > 50, _ > 500, _ > 5000,
    _ < -20, _ < -200, _ < -2000,
    _ == 0,
    n => n > -40 && n < 40,
    n => n > -80 && n < -10,
    n => n > 110 && n < 150
  )
  def reverseMapFunctions = List(-_, n => n * n, _ + 1)
  def sameElementsSeqs = List(
    List[Int](),
    List(1),
    List(1, 2, 3, 4, 5, 6, 7, 8, 9),
    Array.fill(150)(1).toSeq,
    Array.fill(1000)(1).toSeq
  )
  def startEndSeqs = List(
    Nil,
    List(1),
    List(1, 2, 3, 4, 5),
    List(0, 1, 2, 3, 4, 5),
    List(4, 5, 6, 7, 8, 9, 10),
    List(4, 5, 6, 7, 8, 9, 0),
    List(-4, -3, -2, -1)
  )    
}



















