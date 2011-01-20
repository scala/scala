/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala

/** <p>
 *    An annotation that gets applied to a selector in a match expression.
 *    If it is present, exhaustiveness warnings for that expression will be
 *    suppressed.
 *  </p>
 *  <p>
 *    For example, compiling the code:
 *  </p><pre>
 *    <b>object</b> test <b>extends</b> Application {
 *      <b>def</b> f(x: Option[int]) = x <b>match</b> {
 *        <b>case</b> Some(y) => y
 *      }
 *      f(None)
 *    }</pre>
 *  <p>
 *    will display the following warning:
 *  </p><pre>
 *    test.scala:2: warning: does not cover case {object None}
 *      def f(x: Option[int]) = x match {
 *                              ^
 *    one warning found</pre>
 *  <p>
 *    The above message may be suppressed by substituting the expression
 *    <code>x</code> with <code>(x: @unchecked)</code>. Then the
 *    modified code will compile silently, but, in any case, a
 *    <a href="MatchError.html"><code>MatchError</code></a>
 *    will be raised at runtime.
 *  </p>
 *
 *  @since 2.4
 */
class unchecked extends annotation.Annotation {}
