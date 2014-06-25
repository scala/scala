package scala.collection

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

@RunWith(classOf[JUnit4])
class SeqTest {

  case class Person(fName: String, lName: String, age: Int)

  val eugeneP = Person("Eugene", "Platonov", 27)
  val xeniya = Person("Xeniya", "Skrypnyk", 27)
  val eugeneM = Person("Eugene", "Medvediev", 31)
  val vasiliy = Person("Vasiliy", "Platonov", 2)

  @Test
  def testDistinctBy(): Unit = {
    val people = Seq(eugeneP, xeniya, eugeneM, vasiliy)

    assert(people.distinctBy(_.fName) == Seq(eugeneP, xeniya, vasiliy))
    assert(people.distinctBy(_.lName) == Seq(eugeneP, xeniya, eugeneM))
    assert(people.distinctBy(_.age) == Seq(eugeneP, eugeneM, vasiliy))
  }

  @Test
  def testDistinctBy_EvaluateFunctionOnlyOnce(): Unit = {

    def assertEvaluateOnlyOnce(emptySeq: Seq[Person]) {
      val people = emptySeq ++ Iterable(eugeneP, xeniya, eugeneM, vasiliy)
      var x = 0
      assert(people.distinctBy { p => x += 1; p.fName}.toList == List(eugeneP, xeniya, vasiliy))
      assert(x == 4, "Looks like function has been evaluated wrong number of times")
    }

    assertEvaluateOnlyOnce(Seq.empty[Person])
    assertEvaluateOnlyOnce(Stream.empty[Person])
  }

  @Test
  def testDistinct(): Unit = {
    val people = Seq(eugeneP, xeniya, eugeneM, vasiliy)

    assert(people.map(_.fName).distinct == Seq(eugeneP.fName, xeniya.fName, vasiliy.fName))
    assert(people.map(_.lName).distinct == Seq(eugeneP.lName, xeniya.lName, eugeneM.lName))
    assert(people.map(_.age).distinct == Seq(eugeneP.age, eugeneM.age, vasiliy.age))
  }

  @Test
  def testDistinctByDistinctEmptySeq(): Unit = {
    val empty = Seq.empty[Person]
    assert(empty.distinctBy(_.lName) == empty)
    assert(empty.distinct == empty)
  }
}
