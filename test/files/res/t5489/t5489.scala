package repro

trait HasString {
  def blerg(): String
}

class CausesProblems {
  def problems = (
    if ("don't optimize me away!".length == 0)
      new HasString { def blerg() = "wut" }
    else
      new HasString { def blerg() = "okay" }
  ).blerg()
}
