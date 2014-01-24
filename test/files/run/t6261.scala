import scala.collection.immutable._

object Test extends App {

  def test1() {
    // test that a HashTrieMap with one leaf element is not created!
    val x = HashMap.empty + (1->1) + (2->2)
    if(x.getClass.getSimpleName != "HashTrieMap")
      println("A hash map containing two non-colliding values should be a HashTrieMap")

    val y = x - 1
    if(y.getClass.getSimpleName != "HashMap1")
      println("A hash map containing one element should always use HashMap1")
  }

  def test2() {
    // class that always causes hash collisions
    case class Collision(value:Int) { override def hashCode = 0 }

    // create a set that should have a collison
    val x = HashMap.empty + (Collision(0)->0) + (Collision(1) ->0)
    if(x.getClass.getSimpleName != "HashMapCollision1")
      println("HashMap of size >1 with collisions should use HashMapCollision")

    // remove the collision again by removing all but one element
    val y = x - Collision(0)
    if(y.getClass.getSimpleName != "HashMap1")
      println("HashMap of size 1 should use HashMap1" + y.getClass)
  }
  def test3() {
    // finds an int x such that improved(x) differs in the first bit to improved(0),
    // which is the worst case for the HashTrieSet
    def findWorstCaseInts() {
      // copy of improve from HashSet
      def improve(hcode: Int) = {
        var h: Int = hcode + ~(hcode << 9)
        h = h ^ (h >>> 14)
        h = h + (h << 4)
        h ^ (h >>> 10)
      }

      // find two hashes which have a large separation
      val x = 0
      var y = 1
      val ix = improve(x)
      while(y!=0 && improve(y)!=ix+(1<<31))
        y+=1
      printf("%s %s %x %x\n",x,y,improve(x), improve(y))
    }
    // this is not done every test run since it would slow down ant test.suite too much.
    // findWorstCaseInts()

    // two numbers that are immediately adiacent when fed through HashSet.improve
    val h0 = 0
    val h1 = 1270889724

    // h is the hashcode, i is ignored for the hashcode but relevant for equality
    case class Collision(h:Int, i:Int) {
      override def hashCode = h
    }
    val a = Collision(h0,0)->0
    val b = Collision(h0,1)->0
    val c = Collision(h1,0)->0

    // create a HashSetCollision1
    val x = HashMap(a) + b
    if(x.getClass.getSimpleName != "HashMapCollision1")
      println("x should be a HashMapCollision")
    StructureTests.validate(x)
    //StructureTests.printStructure(x)
    require(x.size==2 && x.contains(a._1) && x.contains(b._1))

    // go from a HashSetCollision1 to a HashTrieSet with maximum depth
    val y = x + c
    if(y.getClass.getSimpleName != "HashTrieMap")
      println("y should be a HashTrieMap")
    StructureTests.validate(y)
    // StructureTests.printStructure(y)
    require(y.size==3 && y.contains(a._1) && y.contains(b._1) && y.contains(c._1))

    // go from a HashSet1 directly to a HashTrieSet with maximum depth
    val z = HashMap(a) + c
    if(y.getClass.getSimpleName != "HashTrieMap")
      println("y should be a HashTrieMap")
    StructureTests.validate(z)
    // StructureTests.printStructure(z)
    require(z.size == 2 && z.contains(a._1) && z.contains(c._1))
  }
  test1()
  test2()
  test3()
}


package scala.collection.immutable {
  object StructureTests {
    def printStructure(x:HashMap[_,_], prefix:String="") {
      x match {
        case m:HashMap.HashTrieMap[_,_] =>
          println(prefix+m.getClass.getSimpleName + " " + m.size)
          m.elems.foreach(child => printStructure(child, prefix + "  "))
        case m:HashMap.HashMapCollision1[_,_] =>
          println(prefix+m.getClass.getSimpleName + " " + m.kvs.size)
        case m:HashMap.HashMap1[_,_] =>
          println(prefix+m.getClass.getSimpleName + " " + m.head)
        case _ =>
          println(prefix+"empty")
      }
    }

    def validate(x:HashMap[_,_]) {
      x match {
        case m:HashMap.HashTrieMap[_,_] =>
          require(m.elems.size>1 || (m.elems.size==1 && m.elems(0).isInstanceOf[HashMap.HashTrieMap[_,_]]))
          m.elems.foreach(validate _)
        case m:HashMap.HashMapCollision1[_,_] =>
          require(m.kvs.size>1)
        case m:HashMap.HashMap1[_,_] =>
        case _ =>
      }
    }
  }
}
