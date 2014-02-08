object Test {
  // The error message tells us that AnyRef#== and Any#== are overloaded.
  // A real class couldn't define such an overload, why do we allow AnyRef
  // to do so?
  "".==[Int]
  ("": AnyRef).==[Int]
  ("": Object).==[Int]
}
