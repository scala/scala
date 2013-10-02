import scala.collection.generic.{Growable, Shrinkable}
import scala.collection.GenSet
import scala.collection.mutable.FlatHashTable
import scala.collection.mutable.HashSet
import scala.collection.parallel.mutable.ParHashSet

object Test extends App {
  test(new Creator{
    def create[A] = new HashSet[A]
    def hashSetType = "HashSet"
  })

  test(new Creator{
    def create[A] = new ParHashSet[A]
    def hashSetType = "ParHashSet"
  })


  def test(creator : Creator) {
    println("*** " + creator.hashSetType + " primitives")
    val h1 = creator.create[Int]
    for (i <- 0 until 20) h1 += i
    println((for (i <- 0 until 20) yield i + " " + (h1 contains i)).toList.sorted mkString(","))
    println((for (i <- 20 until 40) yield i + " " + (h1 contains i)).toList.sorted mkString(","))
    println(h1.toList.sorted mkString ",")
    println

    println("*** " + creator.hashSetType + " Strings with null")
    val h2 = creator.create[String]
    h2 += null
    for (i <- 0 until 20) h2 +=  "" + i
    println("null " + (h2 contains null))
    println((for (i <- 0 until 20) yield i + " " + (h2 contains ("" + i))).toList.sorted mkString(","))
    println((for (i <- 20 until 40) yield i + " " + (h2 contains ("" + i))).toList.sorted mkString(","))
    println((h2.toList map {x => "" + x}).sorted mkString ",")

    h2 -= null
    h2 -= "" + 0
    println("null " + (h2 contains null))
    println((for (i <- 0 until 20) yield i + " " + (h2 contains ("" + i))).toList.sorted mkString(","))
    println
  }

   trait Creator {
     def create[A] : GenSet[A] with Cloneable with FlatHashTable[A] with Growable[A] with Shrinkable[A]
     def hashSetType : String
  }
}