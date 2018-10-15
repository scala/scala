object Test extends App {
  assert(Option.whenNonNull(42) contains 42)
  assert(Some(42) contains 42)
  assert(Some(BigInt(42)) contains 42)
  assert(Some(42) contains BigInt(42))
  assert(!(None contains 42))
  assert(Some(null) contains null)
  assert(!(Option.whenNonNull(null) contains null))
}