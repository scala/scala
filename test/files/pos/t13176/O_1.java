package p;

// Resolving `O.In` as the parent of `Impl` loads `O`, whose sealed hierarchy leads back to `Impl`
abstract sealed class O permits O.In {
  static abstract sealed class In extends O permits Impl {}
}

final class Impl extends O.In {}
