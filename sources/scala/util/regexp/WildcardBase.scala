// $Id$

package scala.util.regexp ;

/** for use as a mixin to add wildcards to the language */

trait WildcardBase extends Base {
  type regexp <: RegExp;
  case object Wildcard                       extends RegExp {
    final val isNullable = false;
  }
}
