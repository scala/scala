object Main {
  def g: BigInt = 5 + BigInt(4) // since we're looking for an implicit that converts an int into something that has a + method that takes a BigInt, BigInt should be in the implicit scope
  def g2 = 5 + BigInt(4)
}