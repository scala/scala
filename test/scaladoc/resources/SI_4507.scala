/**
 * <pre>
 * scala> (new AndOrSpec).execute()
 * AndOrSpec:
 * The ScalaTest Matchers DSL
 *   should provide
 *     an and operator that
 *     - returns silently when evaluating true and true
 *     - throws a TestFailedException when evaluating true and false
 *     - that throws a TestFailedException when evaluating false and true
 *     - throws a TestFailedException when evaluating false and false
 *     an or operator that
 *     - returns silently when evaluating true or true
 *     - returns silently when evaluating true or false
 *     - returns silently when evaluating false or true
 *     - throws a TestFailedException when evaluating false or false
 * </pre>
 */
class SI_4507
