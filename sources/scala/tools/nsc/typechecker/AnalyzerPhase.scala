package scala.tools.nsc.typechecker;

/** The main attribution phase.
 */
abstract class AnalyzerPhase(prev: Phase) extends Analyzer {
  def name = "analyzer";
  def apply(unit: global.CompilationUnit): unit = {
    unit.body = new Analyzer(unit).parse();
  }


