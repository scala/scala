package scala.tools.scalac.transformer.matching ;

abstract class PatternTest ;

/** test for patterns _:T      */
case class TypeTest extends PatternTest ;

/** test for patterns A(...)   */
case class CaseTest extends PatternTest ;

/** test for constant patterns */
case class EqTest extends PatternTest ;
