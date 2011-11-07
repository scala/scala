object A {
  def main(args: Array[String]) = {
    val x = new { def copy(a : this.type) = a };
    x.copy(x)    
  }
}
