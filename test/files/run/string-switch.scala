// scalac: -Werror
import annotation.switch
import util.Try

object Test extends App {

  def species(name: String) = (name.toLowerCase : @switch) match {
    case "fido"                     => "dog"
    case "garfield" | "deuteronomy" => "cat"
    case "wanda"                    => "fish"
    case "henry"                    => "horse"
  }
  List("fido", "garfield", "wanda", "henry", "felix", "deuteronomy").foreach { n => println(s"$n ${Try(species(n))}") }

  println("=====")

  def collide(in: String) = (in : @switch) match {
    case "AaAa" => 1
    case "BBBB" => 2
    case "cCCc" => 3
    case x if x == "ddDd" => 4
  }
  List("AaAa", "BBBB", "BBAa", "cCCc", "ddDd", "EEee").foreach { s =>
    println(s"$s ${s.##} ${Try(collide(s))}")
  }

  println("=====")

  def unitary(in: String) = (in : @switch) match {
    case "A" =>
    case x   => throw new MatchError(x)
  }
  List("A","X").foreach { s =>
    println(s"$s ${Try(unitary(s))}")
  }

  println("=====")

  def nullFun(in: String) = (in : @switch) match {
    case "1" => 1
    case null => 2
    case "" => 3
  }
  List("", null, "7").foreach { s =>
    println(s"$s ${Try(nullFun(s))}")
  }

  println("=====")

  def default(in: String) = (in : @switch) match {
    case "pig" => 1
    case _     => 2
  }
  List("pig","dog").foreach { s =>
    println(s"$s ${Try(default(s))}")
  }

  println("=====")

  def onceOnly(in: Iterator[String]) = (in.next() : @switch) match {
    case "Ea" => 1
    case "FB" => 2 //collision with above
    case "cC" => 3
    case _    => 4
  }
  List("Ea", "FB", "cC", "xx", null).foreach { s =>
    println(s"$s ${s.##} ${Try(onceOnly(Iterator(s)))}")
  }
}
