/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Jason Zaugg
 */

package scala.tools.nsc
package backend.jvm
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.symtab._

/** Code shared between the legagy backend [[scala.tools.nsc.backend.jvm.GenJVM]]
  * and the new backend [[scala.tools.nsc.backend.jvm.GenASM]]. There should be
  * more here, but for now I'm starting with the refactorings that are either
  * straightforward to review or necessary for maintenance.
  */
trait GenJVMASM {
  val global: Global
  import global._
  import icodes._
  import definitions._

  protected def outputDirectory(sym: Symbol): AbstractFile =
    settings.outputDirs outputDirFor beforeFlatten(sym.sourceFile)

  protected def getFile(base: AbstractFile, clsName: String, suffix: String): AbstractFile = {
    var dir = base
    val pathParts = clsName.split("[./]").toList
    for (part <- pathParts.init) {
      dir = dir.subdirectoryNamed(part)
    }
    dir.fileNamed(pathParts.last + suffix)
  }
  protected def getFile(sym: Symbol, clsName: String, suffix: String): AbstractFile =
    getFile(outputDirectory(sym), clsName, suffix)

  protected val ExcludedForwarderFlags = {
    import Flags._
    // Should include DEFERRED but this breaks findMember.
    ( CASE | SPECIALIZED | LIFTED | PROTECTED | STATIC | EXPANDEDNAME | BridgeAndPrivateFlags | MACRO )
  }

  protected def isJavaEntryPoint(icls: IClass) = {
    val sym = icls.symbol
    def fail(msg: String, pos: Position = sym.pos) = {
      icls.cunit.warning(sym.pos,
        sym.name + " has a main method with parameter type Array[String], but " + sym.fullName('.') + " will not be a runnable program.\n" +
          "  Reason: " + msg
        // TODO: make this next claim true, if possible
        //   by generating valid main methods as static in module classes
        //   not sure what the jvm allows here
        // + "  You can still run the program by calling it as " + sym.javaSimpleName + " instead."
      )
      false
    }
    def failNoForwarder(msg: String) = {
      fail(msg + ", which means no static forwarder can be generated.\n")
    }
    val possibles = if (sym.hasModuleFlag) (sym.tpe nonPrivateMember nme.main).alternatives else Nil
    val hasApproximate = possibles exists { m =>
      m.info match {
        case MethodType(p :: Nil, _) => p.tpe.typeSymbol == ArrayClass
        case _                       => false
      }
    }
    // At this point it's a module with a main-looking method, so either succeed or warn that it isn't.
    hasApproximate && {
      // Before erasure so we can identify generic mains.
      beforeErasure {
        val companion     = sym.linkedClassOfClass
        val companionMain = companion.tpe.member(nme.main)

        if (hasJavaMainMethod(companion))
          failNoForwarder("companion contains its own main method")
        else if (companion.tpe.member(nme.main) != NoSymbol)
          // this is only because forwarders aren't smart enough yet
          failNoForwarder("companion contains its own main method (implementation restriction: no main is allowed, regardless of signature)")
        else if (companion.isTrait)
          failNoForwarder("companion is a trait")
        // Now either succeeed, or issue some additional warnings for things which look like
        // attempts to be java main methods.
        else (possibles exists isJavaMainMethod) || {
          possibles exists { m =>
            m.info match {
              case PolyType(_, _) =>
                fail("main methods cannot be generic.")
              case MethodType(params, res) =>
                if (res.typeSymbol :: params exists (_.isAbstractType))
                  fail("main methods cannot refer to type parameters or abstract types.", m.pos)
                else
                  isJavaMainMethod(m) || fail("main method must have exact signature (Array[String])Unit", m.pos)
              case tp =>
                fail("don't know what this is: " + tp, m.pos)
            }
          }
        }
      }
    }
  }
}
