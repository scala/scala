/* NSC -- new scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend
package icode

/** Glue together ICode parts.
 *
 *  @author Iulian Dragos
 */
abstract class ICodes extends AnyRef
                                 with Members
                                 with TypeKinds
                                 with Primitives
{
  val global: Global
  import global.definitions

  lazy val AnyRefReference: TypeKind    = REFERENCE(definitions.AnyRefClass)
  lazy val BoxedUnitReference: TypeKind = REFERENCE(definitions.BoxedUnitClass)
  lazy val NothingReference: TypeKind   = REFERENCE(definitions.NothingClass)
  lazy val NullReference: TypeKind      = REFERENCE(definitions.NullClass)
  lazy val ObjectReference: TypeKind    = REFERENCE(definitions.ObjectClass)
  lazy val StringReference: TypeKind    = REFERENCE(definitions.StringClass)
}

