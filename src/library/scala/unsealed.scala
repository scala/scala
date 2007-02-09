/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: remote.scala 9400 2006-11-28 17:22:45 +0000 (Tue, 28 Nov 2006) michelou $


package scala

/**
 * An annotation that gets applied to a selector in a match expression.
 * If it is present, exhaustiveness warnings for that expression will be suppressed.
 */
class unsealed extends Annotation {}
