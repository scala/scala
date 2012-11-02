/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.annotation

/** A marker for annotations that, when applied to a type, should be treated
 *  as a constraint on the annotated type.
 *
 *  A proper constraint should restrict the type based only on information
 *  mentioned within the type.  A Scala compiler can use this assumption to
 *  rewrite the contents of the constraint as necessary.  To contrast, a type
 *  annotation whose meaning depends on the context where it is written
 *  down is not a proper constrained type, and this marker should not be
 *  applied.  A Scala compiler will drop such annotations in cases where it
 *  would rewrite a type constraint.
 *
 *  @author  Lex Spoon
 *  @version 1.1, 2007-11-5
 *  @since   2.6
 */
trait TypeConstraint extends Annotation
