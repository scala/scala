trait A { type Result }

class PolyTests {
  def wrong(x: A { type Result = Int })
             : A { type Result = String} = x
}