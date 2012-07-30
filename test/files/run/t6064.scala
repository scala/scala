object Test extends App {
  assert(Option(42) contains 42)
  assert(Some(42) contains 42)
  assert(Option(BigInt(42)) contains 42)
  assert(Option(42) contains BigInt(42))
  assert(!(None contains 42))
  assert(Some(null) contains null)
  assert(!(Option(null) contains null))
}