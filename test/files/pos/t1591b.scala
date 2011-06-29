import scala.tools.nsc._

class SemanticTokens(val compiler: Global) {
  import compiler._

  def build() = ErrorType

  class Process {
    def f() = analyzer
    // or to crash the compiler instead of a nice message,
    // def f() = analyzer underlying _
  }
}
