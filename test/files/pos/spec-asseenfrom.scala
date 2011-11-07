class Automaton[@specialized(Double) W,State] { 

  def finalWeight(s: State): W = error("todo");

  def allStates: Set[State] = error("toodo");

  /**
   * Returns a map from states to its final weight. may expand all nodes.
   */
  def finalStateWeights = Map.empty ++ allStates.map { s => (s,finalWeight(s)) }

  // This works fine:
  /*
  def finalStateWeights() = {
    val it = allStates.iterator;
    while(it.hasNext) {
      finalWeight(it.next);
    }
  }
  */

}

abstract class Automaton2[@specialized T1, T2] {
  def finalWeight(s: T2): T1
  def allStates: Set[T2]

  def f = allStates map finalWeight
}
