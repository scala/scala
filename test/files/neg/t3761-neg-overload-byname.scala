
package overloadByName

// WWTT = what were they thinking
class WWTT {
  def f(i: Int, j: =>Int) = i+j
  def f(i: =>Int, j: Int) = i*j
}

object Test {
  def main(args: Array[String]) {
    // sut =  system under test
    val sut = new WWTT
    sut.f(1,2)
  }
}
