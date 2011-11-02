abstract class AbsArray[T] {
  def apply(idx: Int): T
  def update(idx: Int, elem: T)
  def length: Int
  def applyByte(idx: Int): Byte = apply(idx).asInstanceOf[Byte]
  def updateByte(idx: Int, elem: Byte) = update(idx, elem.asInstanceOf[T])
  def applyChar(idx: Int): Char = apply(idx).asInstanceOf[Char]
  def updateChar(idx: Int, elem: Char) = update(idx, elem.asInstanceOf[T])
  def applyShort(idx: Int): Short = apply(idx).asInstanceOf[Short]
  def updateShort(idx: Int, elem: Short) = update(idx, elem.asInstanceOf[T])
  def applyInt(idx: Int): Int = apply(idx).asInstanceOf[Int]
  def updateInt(idx: Int, elem: Int) = update(idx, elem.asInstanceOf[T])
  def applyLong(idx: Int): Long = apply(idx).asInstanceOf[Long]
  def updateLong(idx: Int, elem: Long) = update(idx, elem.asInstanceOf[T])
  def applyFloat(idx: Int): Float = apply(idx).asInstanceOf[Float]
  def updateFloat(idx: Int, elem: Float) = update(idx, elem.asInstanceOf[T])
  def applyDouble(idx: Int): Double = apply(idx).asInstanceOf[Double]
  def updateDouble(idx: Int, elem: Double) = update(idx, elem.asInstanceOf[T])
  def applyBoolean(idx: Int): Boolean = apply(idx).asInstanceOf[Boolean]
  def updateBoolean(idx: Int, elem: Boolean) = update(idx, elem.asInstanceOf[T])
  def applyObject(idx: Int): Object = apply(idx).asInstanceOf[Object]
  def updateObject(idx: Int, elem: Object) = update(idx, elem.asInstanceOf[T])
}  

final class IntArray(arr: Array[Int]) extends AbsArray[Int] {
  def apply(idx: Int): Int = applyInt(idx)
  def update(idx: Int, elem: Int) = updateInt(idx, elem)
  override def applyInt(idx: Int): Int = arr(idx)
  override def updateInt(idx: Int, elem: Int) = arr(idx) = elem
  def length: Int = arr.length
}

final class ArraySeq[T](arr: Array[T]) extends AbsArray[T] {
  def apply(idx: Int): T = arr(idx)
  def update(idx: Int, elem: T) = arr(idx) = elem
  def length: Int = arr.length
}

class SpecArray[@specialized T](arr: Array[T]) extends AbsArray[T] {
  def apply(idx: Int): T = arr(idx)
  def update(idx: Int, elem: T) = arr(idx) = elem
  def length: Int = arr.length
}

abstract class Test {
  def sum(): Int
  def modify(i: Int)
  def run() {
    var s = 0
    for (i <- 1 to 1000000) {
      s += sum()
      modify(i)
    }
    println(s)
  }
}

class ScalaSpecTest extends Test {
  val arr = new IntArray(new Array[Int](1000))

  def sum(): Int = {
    var acc = 0
    var i = 0
    while (i < arr.length) { acc = acc + arr.applyInt(i); i += 1 }
    acc
  }
  
  def modify(j: Int) = {
    val base = j * 100 % 1000
    var i = 0
    while (i < 100) {
      arr.updateInt(i + base, arr.applyInt(i + base) + 1)
      i += 1
    }
  }
}
    
class ScalaSpec2Test extends Test {
  val arr: AbsArray[Int] = new IntArray(new Array[Int](1000))

  def sum(): Int = {
    var acc = 0
    var i = 0
    while (i < arr.length) { acc = acc + arr.applyInt(i); i += 1 }
    acc
  }
  
  def modify(j: Int) = {
    val base = j * 100 % 1000
    var i = 0
    while (i < 100) {
      arr.updateInt(i + base, arr.applyInt(i + base) + 1)
      i += 1
    }
  }
}
    
class ScalaWrapTest extends Test {
  val arr: AbsArray[Int] = new ArraySeq(new Array[Int](1000))

  def sum(): Int = {
    var acc = 0
    var i = 0
    while (i < arr.length) { acc = acc + arr.applyInt(i); i += 1 }
    acc
  }
  
  def modify(j: Int) = {
    val base = j * 100 % 1000
    var i = 0
    while (i < 100) {
      arr.updateInt(i + base, arr.applyInt(i + base) + 1)
      i += 1
    }
  }
}
    
class ScalaGenTest extends Test {
  val arr: AbsArray[Integer] = new ArraySeq(new Array[Integer](1000))
  for (i <- 0 until arr.length) arr(i) = new Integer(0)

  def sum(): Int = {
    var acc = 0
    var i = 0
    while (i < arr.length) { acc = acc + arr.apply(i).intValue; i += 1 }
    acc
  }
  
  def modify(j: Int) = {
    val base = j * 100 % 1000
    var i = 0
    while (i < 100) {
      arr.update(i + base, new Integer(arr.apply(i + base).intValue + 1))
      i += 1
    }
  }
}
    
class JavaTest extends Test {
  val arr = new Array[Int](1000)

  def sum(): Int = {
    var acc = 0
    var i = 0
    while (i < arr.length) { acc = acc + arr(i); i += 1 }
    acc
  }
  
  def modify(j: Int) = {
    val base = j * 100 % 1000
    var i = 0
    while (i < 100) {
      arr(i + base) += 1
      i += 1
    }
  }
}

/** Specialized array test. */
class ScalaSpec3Test extends Test {
  val arr: SpecArray[Int] = new SpecArray(new Array[Int](1000))

  def sum(): Int = {
    var acc = 0
    var i = 0
    while (i < arr.length) { acc = acc + arr(i); i += 1 }
    acc
  }
  
  def modify(j: Int) = {
    val base = j * 100 % 1000
    var i = 0
    while (i < 100) {
      arr(i + base) = arr(i + base) + 1
      i += 1
    }
  }
}

object TestJava extends scala.testing.Benchmark {
  def run() {
    (new JavaTest).run()
  }
}

object TestSpec extends scala.testing.Benchmark {
  def run() {
    (new ScalaSpecTest).run()
  }
}
     
object TestSpec2 extends scala.testing.Benchmark {
  def run() {
    (new ScalaSpec2Test).run()
  }
}
     
object TestGen extends scala.testing.Benchmark {
  def run() {
    (new ScalaGenTest).run()
  }
}
     
object TestWrap extends scala.testing.Benchmark {
  def run() {
    (new ScalaWrapTest).run()
  }
}
     
object TestSpec3 extends scala.testing.Benchmark {
  def run() {
    (new ScalaSpec3Test).run()
  }
}
