import scala.tools.nsc._

object Test {

  // works
  def mkReifier(global: Global)(typer: global.analyzer.Typer) = typer

/*
<console>:10: error: not found: value global
      class Reifier(global: Global)(typer: global.analyzer.Typer) { }
*/
  class Reifier(global: Global)(typer: global.analyzer.Typer) { }

}
