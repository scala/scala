
trait Z {
  // extra space made the pattern OK
  def f = <z> {{3}}</z> match { case <z> {{3}}</z> => }

  // lack of space: error: illegal start of simple pattern
  def g = <z>{{3}}</z> match { case <z>{{3}}</z> => }
}

