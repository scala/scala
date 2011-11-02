import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Test {
  val maxListLength = 7 // up to 16, but larger is slower
  var testCount = 0
  
  def checkStackOverflow() = {  
    var xs: List[String] = Nil
    for (i <- 0 until 250000)
        xs = "X" :: xs

    val lowers = xs.mapConserve(_.toLowerCase)
    assert(xs.mapConserve(x => x) eq xs)
  }

  def checkBehaviourUnchanged(input: List[_], oldOutput: List[_], newOutput: List[_]) {
    if (oldOutput eq input)
      assert(newOutput eq oldOutput)
    else {
      assert(newOutput.head == oldOutput.head)
      checkBehaviourUnchanged(input.tail, oldOutput.tail, newOutput.tail)
    }
    testCount += 1
  }

    var callCount = 0
    val lastHexDigit: Function1[BigInt, AnyRef] = { x: BigInt => callCount+=1; if (x < 16) x else x % 16 }

    def main(args: Array[String]) {
        for (length <- 0 to maxListLength;
             bitmap <- 0 until (1 << length);
             data = List.range(0, length) map { x: Int =>
               if ((bitmap & (1 << x)) != 0) BigInt(x+16)
               else BigInt(x)
             })
        {
            // Behaves like map with respect to  ==
            callCount = 0
            val numUnconserved = data.reverse.dropWhile(_ < 16).length
            val result = data mapConserve lastHexDigit
            val mapResult = data map lastHexDigit
            assert(result == mapResult)
            assert((result drop numUnconserved) eq (data drop numUnconserved))
            assert(callCount == 2 * length) // map, mapConserve call transform for each element in the list

            // Behaves like existing mapConserve with respect to  eq
            checkBehaviourUnchanged(data, data mapConserve lastHexDigit, data mapConserve lastHexDigit)
        }
        
        checkStackOverflow();
    }
}