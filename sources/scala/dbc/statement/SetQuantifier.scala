/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.dbc.statement;

/** A set quantifier that defines the collection type of a relation. */
abstract class SetQuantifier {
	/** A SQL-99 compliant string representation of the set quantifier. */
	def sqlString: String;
}

object SetQuantifier {
	/** A set quantifier that defines a relation as being a bag. That means
		* that duplicates are allowed. */
	case object AllTuples extends SetQuantifier {
		/** A SQL-99 compliant string representation of the set quantifier. */
		def sqlString: String = "ALL";
	}
	/** A set quantifier that defines a relation as being a set. That means
		* that duplicates are not allowed and will be pruned. */
	case object DistinctTuples extends SetQuantifier {
		/** A SQL-99 compliant string representation of the set quantifier. */
		def sqlString: String = "DISTINCT";
	}
}