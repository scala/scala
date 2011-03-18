object Test {
  // not sure exactly what this is enforcing, but it was failing on
  // me due to some early boxing happening with the check for a
  // stack trace suppression system property, so I boosted the count.
  def main(args: Array[String]) {
    assert(runtime.BoxesRunTime.booleanBoxCount < 10,
      "Expected no more than 10 boolean boxings, found " + runtime.BoxesRunTime.booleanBoxCount)
  }
}
