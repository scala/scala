object Test {
  trait Engine1

  implicit class EngineTools1[Params, R](e: Engine1) {
    def asRequirement: Requirement1[Params, R] = ???
  }
  trait Requirement1[Params, R] {
    def pathsIncludingSelf: Traversable[List[Reportable1[Params, R]]]
  }
  trait Reportable1[Params, R]

  // "missing parameter type" error was swallowed in 2.11.0 leading to a crash
  // in the backend.
  //
  // This error is itself a regression (or at least a change) in 2.11.0-M7,
  // specifically in SI-7944. The type parameters to the implicit view
  // `EngineTools1` are undetermined, and are now treated as type variables
  // in the expected type of the closure argument to `withFilter`.
  for (path: List[Any] <- (null : Engine1).asRequirement.pathsIncludingSelf.toList) {
    ???
  }
}
