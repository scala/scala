package tastytest

import Names._

object TestNames extends Suite("TestNames") {

  def acceptString(s: String) = s.length

  test("read names") {
    val Some(tom) = Name.read("Tom")
    val _  @ None = Name.read("harry")
  }

  test("initial") {
    val Some(tom) = Name.read("Tom")
    assert(initial(tom) == 'T')
  }

  test("name is a string") {
    val Some(tom) = Name.read("Tom")
    assert(acceptString(tom) === 3)
  }

}
