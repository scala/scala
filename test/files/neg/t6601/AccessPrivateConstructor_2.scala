class AccessPrivateConstructor {
  new PrivateConstructor("") // Scalac should forbid accessing to the private constructor!
}
