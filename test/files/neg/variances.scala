trait Vector[+a] {
  def append(x: Vector[a]): Vector[a]
  private[this] def append3(x: Vector[a]): Vector[a] = append(x)
}

