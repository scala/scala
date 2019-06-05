class Test {
  java.util.stream.Stream.of(1,2,3).map(_.toString) // check that map's type param is inferred (should be String)
}
