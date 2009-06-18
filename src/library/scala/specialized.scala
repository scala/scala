package scala

/** Annotate type parameters on which code should be automatically
 *  specialized. For example:
 *  <code>
 *    class MyList[T @specialized] ...
 *  </code>
 */
class specialized extends StaticAnnotation
