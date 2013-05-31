trait Search[M] {
  def search(input: M): C[Int] = {
    println("Search received: " + input)
    null
  }
}

object StringSearch extends Search[String]

trait C[T]
