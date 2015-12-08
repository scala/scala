/**
 * The only change is in the decision to replace a LOAD_LOCAL(l)
 * in the copy-propagation performed before ClosureElimination.
 *
 * In the general case, the local variable 'l' is connected through
 * an alias chain with other local variables and at the end of the
 * alias chain there may be a Value, call it 'v'.
 *
 * If 'v' is cheaper to access (it is a Deref(This) or Const(_)), then
 * replace the instruction to load it from the cheaper place.
 * Otherwise, we use the local variable at the end of the alias chain
 * instead of 'l'.
 */

import scala.tools.partest.IcodeComparison

object Test extends IcodeComparison {
  override def printIcodeAfterPhase = "dce"
}

import scala.util.Random._

/**
 * The example in the bug report (Issue-5321): an alias chain which store
 * an Unknown. Should remove local variable 'y'.
 */
object TestBugReport {
  def test(x: Int) = {
    val y = x
    println(y)
  }
}

/**
 * The code taken from scala.tools.nsc.settings.Settings:
 * After inlining of the setter is performed, there is an opportunity for
 * copy-propagation to eliminate some local variables.
 */
object TestSetterInline {
  private var _postSetHook: this.type => Unit = (x: this.type) => ()
  def withPostSetHook(f: this.type => Unit): this.type = { _postSetHook = f ; this }
}


/**
 * The access of the local variable 'y' should be replaced by the
 * constant.
 */
object TestAliasChainConstant {

  def main(args: Array[String]): Unit = {
    val x = 2
    val y = x
    println(y)
  }
}

/**
 * At the end of the alias chain we have a reference to 'this'.
 * The local variables should be all discarded and replace by a
 * direct reference to this
 */
class TestAliasChainDerefThis {

  def main(args: Array[String]): Unit = {
    val x = this
    val y = x
    println(y)
  }
}

/**
 * At the end of the alias chain, there is the value of a field.
 * The use of variable 'y' should be replaced by 'x', not by an access
 * to the field 'f' since it is more costly.
 */
object TestAliasChainDerefField {
  def f = nextInt

  def main(args: Array[String]): Unit = {
    val x = f
    val y = x
    println(y)
  }
}


/**
 * The first time 'println' is called, 'x' is replaced by 'y'
 * and the second time, 'y' is replaced by 'x'. But none of them
 * can be removed.
 */
object TestDifferentBindings {

  def main(args: Array[String]): Unit = {
    var x = nextInt
    var y = x
    println(y)

    y = nextInt
    x = y
    println(x)
  }
}
