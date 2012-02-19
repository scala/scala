trait Chunk[@specialized +A] {
  def bippy[@specialized B >: A](e: B): Chunk[B]
}