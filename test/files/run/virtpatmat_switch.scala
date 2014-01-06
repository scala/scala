object Test extends App {
  def intSwitch(x: Int) = x match {
    case 0 => "zero"
    case 1 => "one"
    case _ => "many"
  }

  println(intSwitch(0))
  println(intSwitch(1))
  println(intSwitch(10))

  def charSwitch(x: Char) = x match {
    case 'a' => "got a"
    case 'b' => "got b"
    case _ => "got some letter"
  }

  def byteSwitch(x: Byte) = x match {
    case 'a' => "got a"
    case 'b' => "got b"
    case _ => "got some letter"
  }

  println(charSwitch('a'))
  println(byteSwitch('b'))
  println(charSwitch('z'))

  def implicitDefault(x: Int) = x match {
    case 0 => 0
  }

  try {
    implicitDefault(5)
  } catch {
    case e: MatchError => println(e)
  }

}
