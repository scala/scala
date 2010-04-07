object Stooges extends Enumeration {
  type Stooge = Value
  val Larry, Curly, Moe = Value
  def nextStooge(v:Stooges.Stooge):Stooges.Stooge =
    Stooges((v.id+1) % Stooges.maxId)
}

object Test {
  def main(args: Array[String]) {
    println(Stooges.Larry)
    println(Stooges.Curly)
    println(Stooges.Moe)
  }
}
