object Test {

  def faculty(x: Int) = if (x == 0) 1 else x * faculty(x - 1)

}
