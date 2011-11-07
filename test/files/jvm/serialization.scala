//############################################################################
// Serialization
//############################################################################

object Serialize {
  @throws(classOf[java.io.IOException])
  def write[A](o: A): Array[Byte] = {
    val ba = new java.io.ByteArrayOutputStream(512)
    val out = new java.io.ObjectOutputStream(ba)
    out.writeObject(o)
    out.close()
    ba.toByteArray()
  }
  @throws(classOf[java.io.IOException])
  @throws(classOf[ClassNotFoundException])
  def read[A](buffer: Array[Byte]): A = {
    val in =
      new java.io.ObjectInputStream(new java.io.ByteArrayInputStream(buffer))
    in.readObject().asInstanceOf[A]
  }
  def check[A, B](x: A, y: B) {
    println("x = " + x)
    println("y = " + y)
    println("x equals y: " + (x equals y) + ", y equals x: " + (y equals x))
    assert((x equals y) && (y equals x))
    println()
  }
}
import Serialize._

//############################################################################
// Test classes in package "scala"

object Test1_scala {

  private def arrayToString[A](arr: Array[A]): String =
    arr.mkString("Array[",",","]")

  private def arrayEquals[A, B](a1: Array[A], a2: Array[B]): Boolean =
    (a1.length == a2.length) &&
    (Iterator.range(0, a1.length) forall { i => a1(i) == a2(i) })

  object WeekDay extends Enumeration {
    type WeekDay = Value
    val Monday, Tuesday, Wednesday, Thusday, Friday, Saturday, Sunday = Value
  }
  import WeekDay._, BigDecimal._, RoundingMode._

  // in alphabetic order
  try {
    // Array
    val a1 = Array(1, 2, 3)
    val _a1: Array[Int] = read(write(a1))
    println("a1 = " + arrayToString(a1))
    println("_a1 = " + arrayToString(_a1))
    println("arrayEquals(a1, _a1): " + arrayEquals(a1, _a1))
    println()

    // Cell
    val c1 = new Cell('a')
    val _c1: Cell[Char] = read(write(c1))
    println("c1 = " + c1)
    println("_c1 = " + _c1)
    println("c1 eq _c1: " + (c1 eq _c1) + ", _c1 eq c1: " + (_c1 eq c1))
    println("c1 equals _c1: " + (c1 equals _c1) + ", _c1 equals c1: " + (_c1 equals c1))
    println()

    // Either
    val e1 = Left(1)
    val _e1: Either[Int, String] = read(write(e1))
    println("e1 = " + e1)
    println("_e1 = " + _e1)
    println("e1 eq _e1: " + (e1 eq _e1) + ", _e1 eq e1: " + (_e1 eq e1))
    println("e1 equals _e1: " + (e1 equals _e1) + ", _e1 equals e1: " + (_e1 equals e1))
    println()

    // Enumeration
    val x7 = BigDecimal.RoundingMode
    val y7: RoundingMode.type = read(write(x7))
    println("x7 = " + x7)
    println("y7 = " + y7)
    println("x7 eq y7: " + (x7 eq y7) + ", y7 eq x7: " + (y7 eq x7))
    println("x7 equals y7: " + (x7 equals y7) + ", y7 equals x7: " + (y7 equals x7))
    println()

    val x8 = WeekDay
    val y8: WeekDay.type = read(write(x8))
    println("x8 = " + x8)
    println("y8 = " + y8)
    println("x8 eq y8: " + (x8 eq y8) + ", y8 eq x8: " + (y8 eq x8))
    println("x8 equals y8: " + (x8 equals y8) + ", y8 equals x8: " + (y8 equals x8))
    println()

    val x9 = UP
    val y9: RoundingMode = read(write(x9))
    println("x9 = " + x9)
    println("y9 = " + y9)
    println("x9 eq y9: " + (x9 eq y9) + ", y9 eq x9: " + (y9 eq x9))
    println("x9 equals y9: " + (x9 equals y9) + ", y9 equals x9: " + (y9 equals x9))
    println()

    val x10 = Monday
    val y10: WeekDay = read(write(x10))
    println("x10 = " + x10)
    println("y10 = " + y10)
    println("x10 eq y10: " + (x10 eq y10) + ", y10 eq x10: " + (y10 eq x10))
    println("x10 equals y10: " + (x10 equals y10) + ", y10 equals x10: " + (y10 equals x10))
    println()

    println("x9 eq x10: " + (x9 eq x10) + ", x10 eq x9: " + (x10 eq x9))
    println("x9 equals x10: " + (x9 equals x10) + ", x10 equals x9: " + (x10 equals x9))
    println("x9 eq y10: " + (x9 eq y10) + ", y10 eq x9: " + (y10 eq x9))
    println("x9 equals y10: " + (x9 equals y10) + ", y10 equals x9: " + (y10 equals x9))
    println()

    // Function
    val f1 = { x: Int => 2 * x }
    val _f1: Function[Int, Int] = read(write(f1))
    println("f1 = <na>")
    println("_f1 = <na>")
    println("f1(2): " + f1(2) + ", _f1(2): " + _f1(2))
    println()

    // List
    val xs0 = List(1, 2, 3)
    val _xs0: List[Int] = read(write(xs0))
    println("xs0 = " + xs0)
    println("_xs0 = " + _xs0)
    println("xs0 eq _xs0: " + (xs0 eq _xs0) + ", _xs0 eq xs0: " + (_xs0 eq xs0))
    println("xs0 equals _xs0: " + (xs0 equals _xs0) + ", _xs0 equals xs0: " + (_xs0 equals xs0))
    println()

    val xs1 = Nil
    val _xs1: List[Nothing] = read(write(xs1))
    println("xs1 = " + xs1)
    println("_xs1 = " + _xs1)
    println("xs1 eq _xs1: " + (xs1 eq _xs1) + ", _xs1 eq xs1: " + (_xs1 eq xs1))
    println()

    // Option
    val o1 = None
    val _o1: Option[Nothing] = read(write(o1))
    println("o1 = " + o1)
    println("_o1 = " + _o1)
    println("o1 eq _o1: " + (o1 eq _o1) + ", _o1 eq o1: " + (_o1 eq o1))
    println()

    val o2 = Some(1)
    val _o2: Option[Int] = read(write(o2))
    println("o2 = " + o2)
    println("_o2 = " + _o2)
    println("o2 eq _o2: " + (o2 eq _o2) + ", _o2 eq o2: " + (_o2 eq o2))
    println("o2 equals _o2: " + (o2 equals _o2) + ", _o2 equals o2: " + (_o2 equals o2))
    println()
/*
    // Responder
    val r1 = Responder.constant("xyz")
    val _r1: Responder[String] = read(write(r1))
    check(r1, _r1)
*/
    // Symbol
    val s1 = 'hello
    val _s1: Symbol = read(write(s1))
    println("s1 = " + s1)
    println("_s1 = " + _s1)
    println("s1 eq _s1: " + (s1 eq _s1) + ", _s1 eq s1: " + (_s1 eq s1))
    println("s1 equals _s1: " + (s1 equals _s1) + ", _s1 equals s1: " + (_s1 equals s1))
    println()

    // Tuple
    val t1 = ("BannerLimit", 12345)
    val _t1: (String, Int) = read(write(t1))
    println("t1 = " + t1)
    println("_t1 = " + _t1)
    println("t1 eq _t1: " + (t1 eq _t1) + ", _t1 eq t1: " + (_t1 eq t1))
    println("t1 equals _t1: " + (t1 equals _t1) + ", _t1 equals t1: " + (_t1 equals t1))
    println()
  }
  catch {
    case e: Exception =>
      println("Error in Test1_scala: " + e)
      throw e
  }
}

//############################################################################
// Test classes in package "scala.collection.immutable"

object Test2_immutable {
  import scala.collection.immutable.{
    BitSet, HashMap, HashSet, ListMap, ListSet, Queue, Range, SortedMap,
    SortedSet, Stack, Stream, TreeMap, TreeSet, Vector}

  // in alphabetic order
  try {
    // BitSet
    val bs1 = BitSet.empty + 1 + 2
    val _bs1: BitSet = read(write(bs1))
    check(bs1, _bs1)

    val bs2 = {
      val bs = new collection.mutable.BitSet()
      bs += 2; bs += 3
      bs.toImmutable
    }
    val _bs2: BitSet = read(write(bs2))
    check(bs2, _bs2)

    // HashMap
    val hm1 = new HashMap[Int, String] + (1 -> "A", 2 -> "B", 3 -> "C")
    val _hm1: HashMap[Int, String] = read(write(hm1))
    check(hm1, _hm1)

    // HashSet
    val hs1 = new HashSet[Int] + 1 + 2
    val _hs1: HashSet[Int] = read(write(hs1))
    check(hs1, _hs1)

    // List
    val xs1 = List(("buffers", 20), ("layers", 2), ("title", 3))
    val _xs1: List[(String, Int)] = read(write(xs1))
    check(xs1, _xs1)

    // ListMap
    val lm1 = new ListMap[String, Int] + ("buffers" -> 20, "layers" -> 2, "title" -> 3)
    val _lm1: ListMap[String, Int] = read(write(lm1))
    check(lm1, _lm1)

    // ListSet
    val ls1 = new ListSet[Int] + 3 + 5
    val _ls1: ListSet[Int] = read(write(ls1))
    check(ls1, _ls1)

    // Queue
    val q1 = Queue("a", "b", "c")
    val _q1: Queue[String] = read(write(q1))
    check(q1, _q1)

    // Range
    val r1 = 0 until 10
    val _r1: Range = read(write(r1))
    check(r1, _r1)

    val r2 = Range.Long(0L, 10L, 1)
    val _r2: r2.type = read(write(r2))
    check(r2, _r2)

    // SortedMap
    val sm1 = SortedMap.empty[Int, String] + (2 -> "B", 3 -> "C", 1 -> "A")
    val _sm1: SortedMap[Int, String] = read(write(sm1))
    check(sm1, _sm1)

    // SortedSet
    val ss1 = SortedSet.empty[Int] + 2 + 3 + 1
    val _ss1: SortedSet[Int] = read(write(ss1))
    check(ss1, _ss1)

    // Stack
    val s1 = new Stack().push("a", "b", "c")
    val _s1: Stack[String] = read(write(s1))
    check(s1, _s1)

    // Stream
    val st1 = Stream.range(0, 10)
    val _st1: Stream[Int] = read(write(st1))
    check(st1, _st1)

    // TreeMap
    val tm1 = new TreeMap[Int, String] + (42 -> "FortyTwo")
    val _tm1: TreeMap[Int, String] = read(write(tm1))
    check(tm1, _tm1)

    // TreeSet
    val ts1 = new TreeSet[Int]() + 2 + 0
    val _ts1: TreeSet[Int] = read(write(ts1))
    check(ts1, _ts1)

    // Vector
    val v1 = Vector('a, 'b, 'c)
    val _v1: Vector[Symbol] = read(write(v1))
    check(v1, _v1)
  }
  catch {
    case e: Exception =>
      println("Error in Test2_immutable: " + e)
      throw e
  }
}

//############################################################################
// Test classes in package "scala.collection.mutable"

object Test3_mutable {
  import scala.reflect.ClassManifest
  import scala.collection.mutable.{
    ArrayBuffer, ArrayBuilder, ArraySeq, ArrayStack, BitSet, DoubleLinkedList,
    HashMap, HashSet, History, LinkedList, ListBuffer, Publisher, Queue,
    Stack, StringBuilder, WrappedArray}

  // in alphabetic order
  try {
    // ArrayBuffer
    val ab1 = new ArrayBuffer[String]
    ab1 ++= List("one", "two")
    val _ab1: ArrayBuffer[String] = read(write(ab1))
    check(ab1, _ab1)
    
    // ArrayBuilder
    val abu1 = ArrayBuilder.make[Long]
    val _abu1: ArrayBuilder[ClassManifest[Long]] = read(write(abu1))
    check(abu1, _abu1)

    val abu2 = ArrayBuilder.make[Float]
    val _abu2: ArrayBuilder[ClassManifest[Float]] = read(write(abu2))
    check(abu2, _abu2)
    
    // ArraySeq
    val aq1 = ArraySeq(1, 2, 3)
    val _aq1: ArraySeq[Int] = read(write(aq1))
    check(aq1, _aq1)
    
    // ArrayStack
    val as1 = new ArrayStack[Int]
    as1 ++= List(20, 2, 3).iterator
    val _as1: ArrayStack[Int] = read(write(as1))
    check(as1, _as1)

    // BitSet
    val bs1 = new BitSet()
    bs1 += 0
    bs1 += 8
    bs1 += 9
    val _bs1: BitSet = read(write(bs1))
    check(bs1, _bs1)
/*
    // DoubleLinkedList
    val dl1 = new DoubleLinkedList[Int](2, null)
    dl1.append(new DoubleLinkedList(3, null))
    val _dl1: DoubleLinkedList[Int] = read(write(dl1))
    check(dl1, _dl1)
*/
    // HashMap
    val hm1 = new HashMap[String, Int]
    hm1 ++= List(("A", 1), ("B", 2), ("C", 3)).iterator
    val _hm1: HashMap[String, Int] = read(write(hm1))
    check(hm1, _hm1)

    // HashSet
    val hs1 = new HashSet[String]
    hs1 ++= List("layers", "buffers", "title").iterator
    val _hs1: HashSet[String] = read(write(hs1))
    check(hs1, _hs1)

    val h1 = new History[String, Int]
    val _h1: History[String, Int] = read(write(h1))
    check(h1, _h1)
/*
    // LinkedList
    val ll1 = new LinkedList[Int](2, null)
    ll1.append(new LinkedList(3, null))
    val _ll1: LinkedList[Int] = read(write(ll1))
    check(ll1, _ll1)
*/
    // ListBuffer
    val lb1 = new ListBuffer[String]
    lb1 ++= List("white", "black")
    val _lb1: ListBuffer[String] = read(write(lb1))
    check(lb1, _lb1)

    // Queue
    val q1 = new Queue[Int]
    q1 ++= List(20, 2, 3).iterator
    val _q1: Queue[Int] = read(write(q1))
    check(q1, _q1)

    // Stack
    val s1 = new Stack[Int]
    s1 pushAll q1
    val _s1: Stack[Int] = read(write(s1))
    check(s1, _s1)

    // StringBuilder
    val sb1 = new StringBuilder
    sb1 append "abc"
    val _sb1: StringBuilder = read(write(sb1))
    check(sb1, _sb1)

    // WrappedArray
    val wa1 = WrappedArray.make(Array(1, 2, 3))
    val _wa1: WrappedArray[Int] = read(write(wa1))
    check(wa1, _wa1)
  }
  catch {
    case e: Exception =>
      println("Error in Test3_mutable: " + e)
      throw e
  }
}


//############################################################################
// Test classes in package "scala.xml"

object Test4_xml {
  import scala.xml.{Attribute, Document, Elem, Null, PrefixedAttribute, Text}

  case class Person(name: String, age: Int)

  try {
    // Attribute
    val a1 = new PrefixedAttribute("xml", "src", Text("hello"), Null)
    val _a1: Attribute = read(write(a1))
    check(a1, _a1)

    // Document
    val d1 = new Document
    d1.docElem = <title></title>
    d1.encoding = Some("UTF-8")
    val _d1: Document = read(write(d1))
    check(d1, _d1)

    // Elem
    val e1 = <html><title>title</title><body></body></html>;
    val _e1: Elem = read(write(e1))
    check(e1, _e1)

    class AddressBook(a: Person*) {
      private val people: List[Person] = a.toList
      def toXHTML =
      <table cellpadding="2" cellspacing="0">
        <tr>
          <th>Last Name</th>
          <th>First Name</th>
        </tr>
        { for (p <- people) yield
        <tr>
          <td> { p.name } </td>
          <td> { p.age.toString() } </td>
        </tr> }
      </table>;
    }

    val people = new AddressBook(
      Person("Tom", 20),
      Person("Bob", 22),
      Person("James", 19))

    val e2 =
      <html>
      <body>
        { people.toXHTML }
      </body>
      </html>;
    val _e2: Elem = read(write(e2))
    check(e2, _e2)
  }
  catch {
    case e: Exception =>
      println("Error in Test4_xml: " + e)
      throw e
  }
}

//############################################################################
// Test user-defined classes WITHOUT nesting

class Person(_name: String) extends Serializable {
  private var name = _name
  override def toString() = name
  override def equals(that: Any): Boolean =
    that.isInstanceOf[Person] &&
    (name == that.asInstanceOf[Person].name)
}

class Employee(_name: String) extends Serializable {
  private var name = _name
  override def toString() = name
}

object bob extends Employee("Bob")

object Test5 {
  val x1 = new Person("Tim")
  val x2 = bob

  try {
    val y1: Person   = read(write(x1))
    val y2: Employee = read(write(x2))

    check(x1, y1)
    check(x2, y2)
  }
  catch {
    case e: Exception =>
      println("Error in Test5: " + e)
  }
}

//############################################################################
// Test user-defined classes WITH nesting

object Test6 {
  object bill extends Employee("Bill") {
    val x = paul
  }
  object paul extends Person("Paul") {
    val x = 4  //  bill; => StackOverflowException !!!
  }
  val x1 = new Person("John")
  val x2 = bill
  val x3 = paul

  try {
    val y1: Person   = read(write(x1))
    val y2: Employee = read(write(x2))
    val y3: Person   = read(write(x3))

    check(x1, y1)
    check(x2, y2)
    check(x3, y3)
  }
  catch {
    case e: Exception =>
      println("Error in Test6: " + e)
  }
}

//############################################################################
// Nested objects cannot get readresolve automatically because after deserialization
// they would be null (they are treated as lazy vals)
class Outer extends Serializable {
    object Inner extends Serializable
}

object Test7 {
  val x = new Outer
  x.Inner // initialize
  try {
    val y:Outer = read(write(x))
    if (y.Inner == null)
      println("Inner object is null")
  }
  catch {
  case e: Exception =>
    println("Error in Test7: " + e)
  }
    
}


// Verify that transient lazy vals don't get serialized
class WithTransient extends Serializable {
  @transient lazy val a1 = 1
  @transient private lazy val a2 = 2
  @transient object B extends Serializable
    
  def test = {
    println(a1)
    println(a2)
    if (B == null)
     println("Transient nested object failed to serialize properly")
  }
}

object Test8 {
    val x = new WithTransient
    x.test
  try {
    val y:WithTransient = read(write(x))
    y.test
  }
  catch {
  case e: Exception =>
    println("Error in Test8: " + e)
  }
}

//############################################################################
// Test code

object Test {
  def main(args: Array[String]) {
    Test1_scala
    Test2_immutable
    Test3_mutable
    Test4_xml
    Test5
    Test6
    Test7
    Test8
    Test9_parallel
  }
}

//############################################################################


//############################################################################
// Test classes in package "scala.collection.parallel" and subpackages
object Test9_parallel {
  import scala.collection.parallel._
  
  try {
    println()
    
    // UnrolledBuffer
    val ub = new collection.mutable.UnrolledBuffer[String]
    ub ++= List("one", "two")
    val _ub: collection.mutable.UnrolledBuffer[String] = read(write(ub))
    check(ub, _ub)
    
    // mutable.ParArray
    val pa = mutable.ParArray("abc", "def", "etc")
    val _pa: mutable.ParArray[String] = read(write(pa))
    check(pa, _pa)
    
    // mutable.ParHashMap
    val mpm = mutable.ParHashMap(1 -> 2, 2 -> 4)
    val _mpm: mutable.ParHashMap[Int, Int] = read(write(mpm))
    check(mpm, _mpm)
    
    // mutable.ParHashSet
    val mps = mutable.ParHashSet(1, 2, 3)
    val _mps: mutable.ParHashSet[Int] = read(write(mps))
    check(mps, _mps)
    
    // immutable.ParRange
    val pr1 = immutable.ParRange(0, 4, 1, true)
    val _pr1: immutable.ParRange = read(write(pr1))
    check(pr1, _pr1)
    
    val pr2 = immutable.ParRange(0, 4, 1, false)
    val _pr2: immutable.ParRange = read(write(pr2))
    check(pr2, _pr2)
    
    // immutable.ParHashMap
    val ipm = immutable.ParHashMap(5 -> 1, 10 -> 2)
    val _ipm: immutable.ParHashMap[Int, Int] = read(write(ipm))
    check(ipm, _ipm)
    
    // immutable.ParHashSet
    val ips = immutable.ParHashSet("one", "two")
    val _ips: immutable.ParHashSet[String] = read(write(ips))
    check(ips, _ips)
    
  } catch {
    case e: Exception =>
      println("Error in Test5_parallel: " + e)
      throw e
  }
}
