//> using options --release 8 -opt:inline:** -Wopt:_ -Werror
//
// JDK methods compiled against ct.sym have no bytecode, the inliner heuristics should not select them
class C {
  def f(s: String) = s.length + Integer.valueOf(1) + Math.max(1, 2) + java.util.Objects.requireNonNull(s).length
}
