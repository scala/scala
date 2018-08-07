object Test extends App {
  assert(Class.forName("Test$delayedInit$body").getEnclosingClass() == null)
}
