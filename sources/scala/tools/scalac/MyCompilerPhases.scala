/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

/*
**       The Global analysis phase.
**
**  We add a new phase in the compiler, WholeProgPhase.
**
** [iuli]   3.03.2004                                                   */

// import scalac.{CompilerPhases => scalac_CompilerPhases}

import scala.tools.scalac.{CompilerPhases => old_CompilerPhases}
import scalac.{Global => scalac_Global}
import scalac.transformer.{ICodePhase => scalac_ICodePhase}
import scalac.PhaseDescriptor;
import scalac.{Unit => scalac_Unit}
import scalac.atree._;
import scala.tools.scalac.wholeprog._;
//import scalac.ast._;
import scalac.util.Name;

package scala.tools.scalac {

/**
 * We add our phase to the compiler phases. To do this, we derive
 * from the current CompilerPhases class and insert our phase after
 * the tail call phase.
 *
 */
class MyCompilerPhases extends old_CompilerPhases {

  var WHOLEPROG: PhaseDescriptor = new PhaseDescriptor(
    "wholeprog",
    "analyze class hierarchy",
    "Find final classes, unused code and monomorphic call-sites",
    WHOLEPROG_PHASE);


  insertAfter(WHOLEPROG, TAILCALL);

  protected def WHOLEPROG_PHASE: Class =
    Class.forName("scala.tools.scalac.WholeProgPhase$class");
}

/**
 * This phase analyzes the whole program and tries to derive some
 * useful facts about it: which classes can be marked final, what
 * methods, fields are never used, and monomorphic call-sites.
 *
 * TODO: Maybe the parent should be something other than ICodePhase!!
 */
class WholeProgPhase(global: scalac_Global, descriptor: PhaseDescriptor)
    extends scalac_ICodePhase (global, descriptor) {


  // ##################################################
  // Public methods

  /* Apply the global analysis phase to the given units */
  override def apply(units: Array[scalac_Unit]): unit = {

    if (!global.args.XdotFile.value.equals("$")) {
      val dotFilePrinter = new PrintDotFile(units);
      dotFilePrinter.makeDotFile(global.args.XdotFile.value);
    }

    if (!global.args.XrootClass.value.equals("$")) {

      var builder: ApplicationBuilder = new ApplicationBuilder(global);
      builder.buildApplication(global.args.XrootClass.value, units);
    }

    if (!global.args.XdotFile.value.equals("$")) {
      val dotFilePrinter = new PrintDotFile(units);
      dotFilePrinter.makeDotFile(global.args.XdotFile.value + "2");
    }
  }

  /** visit each method of a class
    * and log Apply cases           */
  def visitClass(cl: AClass): unit = {
    val methods = cl.methods();

    var i: int = 0;
    while (i < methods.length) {
      visitCode (methods(i).code());
      i = i + 1;
    }
  }

  /** try to find call-sites and print them out */
  def visitCode(code: ACode): unit = {

    code match {
      case ACode$Apply(AFunction$Method(obj, method, style),_,_) => {
	visitCode(obj);
	global.log("   Apply method: " + obj.toString() + "/" + method + " style: " + style);
      }

      case ACode$Apply(_, _, _) => global.log("   Apply something: " + code);

      case ACode$Block(_, stats, _) => {
	stats.foreach( (stat: ACode) => { visitCode(stat) } );
      }

      case ACode$Drop(value, _) => visitCode(value);

      case _ => ();
    }
  }
}

}
