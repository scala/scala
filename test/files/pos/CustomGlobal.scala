package custom

import scala.tools.nsc._, reporters._, typechecker._

/** Demonstration of a custom Global with a custom Typer,
 *  decoupled from trunk.  Demonstration:
 *
{{{
scalac -d . CustomGlobal.scala && scala -nc -Yglobal-class custom.CustomGlobal \
  -e 'class Bippy(x: Int) ; def f = new Bippy(5)'

I'm typing a Bippy! It's a ClassDef.
I'm typing a Bippy! It's a Ident.
I'm typing a Bippy! It's a DefDef.
}}}
 *
 */
class CustomGlobal(currentSettings: Settings, reporter: Reporter) extends Global(currentSettings, reporter) {
  override lazy val analyzer = new {
    val global: CustomGlobal.this.type = CustomGlobal.this
  } with Analyzer {
    override def newTyper(context: Context): Typer = new CustomTyper(context)

    class CustomTyper(context : Context) extends Typer(context) {
      override def typed(tree: Tree, mode: Int, pt: Type): Tree = {
        if (tree.summaryString contains "Bippy")
          println("I'm typing a Bippy! It's a " + tree.shortClass + ".")

        super.typed(tree, mode, pt)
      }
    }
  }
}
