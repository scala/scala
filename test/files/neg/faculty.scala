object Test {

  def faculty(x: int) = if (x == 0) 1 else x * faculty(x - 1)

}
