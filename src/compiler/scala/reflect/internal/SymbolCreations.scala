 /* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.reflect
package internal

import scala.collection.{ mutable, immutable }
import scala.collection.mutable.ListBuffer
import util.Statistics._
import Flags._
import api.Modifier
import scala.tools.util.StringOps.{ ojoin }

trait SymbolCreations {
  self: SymbolTable =>

  import definitions._

  /** Symbol creation interface, possibly better moved somewhere else.
   *  It'd be nice if we had virtual classes, but since we
   *  don't: these methods insulate the direct instantiation of the symbols
   *  (which may be overridden, e.g. in SynchronizedSymbols) from the
   *  enforcement of preconditions and choice of symbol constructor based
   *  on flags, which are (or should be) final so they can be reasoned about
   *  without lots of surprises.
   */
  trait SymbolCreatorInterface {
    // Fallbacks; more precise creators should normally be called.
    protected def createTermSymbol(name: TermName, pos: Position, newFlags: Long): TermSymbol
    // This in fact does not exist anymore in the interests of better typed TypeSymbols.
    // protected def createTypeSymbol(name: TypeName, pos: Position, newFlags: Long): TypeSymbol

    // I believe all but rogue TypeSymbols are one of: ClassSymbol, AbstractTypeSymbol, AliasTypeSymbol, or TypeSkolem.
    protected def createAbstractTypeSymbol(name: TypeName, pos: Position, newFlags: Long): AbstractTypeSymbol
    protected def createAliasTypeSymbol(name: TypeName, pos: Position, newFlags: Long): AliasTypeSymbol
    protected def createTypeSkolemSymbol(name: TypeName, origin: AnyRef, pos: Position, newFlags: Long): TypeSkolem
    protected def createClassSymbol(name: TypeName, pos: Position, newFlags: Long): ClassSymbol

    // More specific ClassSymbols.
    // TODO - AnonymousClassSymbol.
    // TODO maybe - PackageObjects, but that one cost me a lot of time when I tried it before
    // because it broke reification some way I couldn't see.
    protected def createModuleClassSymbol(name: TypeName, pos: Position, newFlags: Long): ModuleClassSymbol
    protected def createPackageClassSymbol(name: TypeName, pos: Position, newFlags: Long): PackageClassSymbol
    protected def createRefinementClassSymbol(pos: Position, newFlags: Long): RefinementClassSymbol
    protected def createImplClassSymbol(name: TypeName, pos: Position, newFlags: Long): ClassSymbol
    protected def createPackageObjectClassSymbol(pos: Position, newFlags: Long): PackageObjectClassSymbol

    // Distinguished term categories include methods, modules, packages, package objects,
    // value parameters, and values (including vals, vars, and lazy vals.)
    protected def createMethodSymbol(name: TermName, pos: Position, newFlags: Long): MethodSymbol
    protected def createModuleSymbol(name: TermName, pos: Position, newFlags: Long): ModuleSymbol
    protected def createPackageSymbol(name: TermName, pos: Position, newFlags: Long): PackageSymbol

    // TODO
    // protected def createValueParameterSymbol(name: TermName, pos: Position, newFlags: Long): TermSymbol
    // protected def createValueMemberSymbol(name: TermName, pos: Position, newFlags: Long): TermSymbol
  }

  trait SymbolCreator extends SymbolCreatorInterface {
    self: Symbol =>

    /*** Predictable symbol creation.
     *
     *   newTermSymbol, newClassSymbol, and newNonClassSymbol all create symbols based
     *   only on the flags (for reconstruction after reification.) It would be nice to
     *   combine the last two into newTypeSymbol, but this requires some flag which allows us
     *   to distinguish classes and type aliases, which as yet does not exist.
     *
     *   The fundamental flags used to determine which Symbol subclass to instantiate are:
     *     METHOD, PACKAGE, MODULE, PARAM, DEFERRED.
     */
    final def newTermSymbol(name: TermName, pos: Position = NoPosition, newFlags: Long = 0L): TermSymbol = {
      if ((newFlags & METHOD) != 0)
        createMethodSymbol(name, pos, newFlags)
      else if ((newFlags & PACKAGE) != 0)
        createPackageSymbol(name, pos, newFlags | PackageFlags)
      else if ((newFlags & MODULE) != 0)
        createModuleSymbol(name, pos, newFlags)
      else if ((newFlags & PARAM) != 0)
        createValueParameterSymbol(name, pos, newFlags)
      else
        createValueMemberSymbol(name, pos, newFlags)
    }

    final def newClassSymbol(name: TypeName, pos: Position = NoPosition, newFlags: Long = 0L): ClassSymbol = {
      if (name == tpnme.REFINE_CLASS_NAME)
        createRefinementClassSymbol(pos, newFlags)
      else if ((newFlags & PACKAGE) != 0)
        createPackageClassSymbol(name, pos, newFlags | PackageFlags)
      else if (name == tpnme.PACKAGE)
        createPackageObjectClassSymbol(pos, newFlags)
      else if ((newFlags & MODULE) != 0)
        createModuleClassSymbol(name, pos, newFlags)
      else if ((newFlags & IMPLCLASS) != 0)
        createImplClassSymbol(name, pos, newFlags)
      else
        createClassSymbol(name, pos, newFlags)
    }

    final def newNonClassSymbol(name: TypeName, pos: Position = NoPosition, newFlags: Long = 0L): TypeSymbol = {
      if ((newFlags & DEFERRED) != 0)
        createAbstractTypeSymbol(name, pos, newFlags)
      else
        createAliasTypeSymbol(name, pos, newFlags)
    }

    def newTypeSymbol(name: TypeName, pos: Position = NoPosition, newFlags: Long = 0L): TypeSymbol =
      newNonClassSymbol(name, pos, newFlags)
  }
}
