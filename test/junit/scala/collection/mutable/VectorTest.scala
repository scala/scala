package scala.collection.mutable

import org.junit.jupiter.api.Test

/* Test for scala/bug#8014 and ++ in general  */
class VectorTest {
  val noVec = Vector.empty[Int]
  val smallVec = Vector.range(0,3)
  val bigVec = Vector.range(0,64)
  val smsm = Vector.tabulate(2 * smallVec.length)(i => (i % smallVec.length))
  val smbig = Vector.tabulate(smallVec.length + bigVec.length)(i => 
    if (i < smallVec.length) i else i - smallVec.length
  )
  val bigsm = Vector.tabulate(smallVec.length + bigVec.length)(i => 
    if (i < bigVec.length) i else i - bigVec.length
  )
  val bigbig = Vector.tabulate(2 * bigVec.length)(i => (i % bigVec.length))


  val vecs = List(noVec, smallVec, bigVec)
  val ans = List(
    vecs,
    List(smallVec, smsm, smbig),
    List(bigVec, bigsm, bigbig)
  )

  @Test
  def vectorCat(): Unit = {
    val cats = vecs.map(a => vecs.map(a ++ _))
    assert( cats == ans )
  }

  @deprecated("Tests deprecated API", since="2.13")
  @Test
  def iteratorCat(): Unit = {
    def its = vecs.map(_.toList.toIterator)
    val cats = vecs.map(a => its.map(x => a ++ x.toList))
    assert( cats == ans )
  }

  @Test
  def arrayCat(): Unit = {
    val ars = vecs.map(_.toArray)
    val cats = vecs.map(a => ars.map(a ++ _))
    assert( cats == ans )
  }
}
