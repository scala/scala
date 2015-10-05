/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala
package tools
package nsc
package transform
package patmat

/** An extractor returns: F1, F2, ..., Fi, opt[Seq[E] or E*]
 *        A case matches: P1, P2, ..., Pj, opt[Seq[E]]
 *          Put together: P1/F1, P2/F2, ... Pi/Fi, Pi+1/E, Pi+2/E, ... Pj/E, opt[Seq[E]]
 *
 *  Here Pm/Fi is the last pattern to match the fixed arity section.
 *
 *    productArity: the value of i, i.e. the number of non-sequence types in the extractor
 *    nonStarArity: the value of j, i.e. the number of non-star patterns in the case definition
 *    elementArity: j - i, i.e. the number of non-star patterns which must match sequence elements
 *       starArity: 1 or 0 based on whether there is a star (sequence-absorbing) pattern
 *      totalArity: nonStarArity + starArity, i.e. the number of patterns in the case definition
 *
 *  Note that productArity is a function only of the extractor, and
 *  nonStar/star/totalArity are all functions of the patterns. The key
 *  value for aligning and typing the patterns is elementArity, as it
 *  is derived from both sets of information.
 */
trait PatternExpander[Pattern, Type] {
  /** You'll note we're not inside the cake. "Pattern" and "Type" are
   *  arbitrary types here, and NoPattern and NoType arbitrary values.
   */
  def NoPattern: Pattern
  def NoType: Type

  /** It's not optimal that we're carrying both sequence and repeated
   *  type here, but the implementation requires more unraveling before
   *  it can be avoided.
   *
   *  sequenceType is Seq[T], elementType is T, repeatedType is T*.
   */
  sealed case class Repeated(sequenceType: Type, elementType: Type, repeatedType: Type) {
    def exists = elementType != NoType

    def elementList  = if (exists) elementType :: Nil else Nil
    def sequenceList = if (exists) sequenceType :: Nil else Nil
    def repeatedList = if (exists) repeatedType :: Nil else Nil

    override def toString = s"${elementType}*"
  }
  object NoRepeated extends Repeated(NoType, NoType, NoType) {
    override def toString = "<none>"
  }

  final case class Patterns(fixed: List[Pattern], star: Pattern) {
    def hasStar      = star != NoPattern
    def starArity    = if (hasStar) 1 else 0
    def nonStarArity = fixed.length
    def totalArity   = nonStarArity + starArity
    def starPatterns = if (hasStar) star :: Nil else Nil
    def all          = fixed ::: starPatterns

    override def toString = all mkString ", "
  }

  /** An 'extractor' can be a case class or an unapply or unapplySeq method.
   *  Decoding what it is that they extract takes place before we arrive here,
   *  so that this class can concentrate only on the relationship between
   *  patterns and types.
   *
   *  In a case class, the class is the unextracted type and the fixed and
   *  repeated types are derived from its constructor parameters.
   *
   *  In an unapply, this is reversed: the parameter to the unapply is the
   *  unextracted type, and the other types are derived based on the return
   *  type of the unapply method.
   *
   *  In other words, this case class and unapply are encoded the same:
   *
   *    case class Foo(x: Int, y: Int, zs: Char*)
   *    def unapplySeq(x: Foo): Option[(Int, Int, Seq[Char])]
   *
   *  Both are Extractor(Foo, Int :: Int :: Nil, Repeated(Seq[Char], Char, Char*))
   *
   *  @param  whole     The type in its unextracted form
   *  @param  fixed     The non-sequence types which are extracted
   *  @param  repeated  The sequence type which is extracted
   */
  final case class Extractor(whole: Type, fixed: List[Type], repeated: Repeated, typeOfSinglePattern: Type) {
    require(whole != NoType, s"expandTypes($whole, $fixed, $repeated)")

    /** A pattern with arity-1 that doesn't match the arity of the Product-like result of the `get` method,
      * will match that result in its entirety. Example:
      *
      * {{{
      * warning: there was one deprecation warning; re-run with -deprecation for details
      * scala> object Extractor { def unapply(a: Any): Option[(Int, String)] = Some((1, "2")) }
      * defined object Extractor
      *
      * scala> "" match { case Extractor(x: Int, y: String) => }
      *
      * scala> "" match { case Extractor(xy : (Int, String)) => }
      * warning: there was one deprecation warning; re-run with -deprecation for details
      * }}}
      * */
    def asSinglePattern: Extractor = copy(fixed = List(typeOfSinglePattern))

    def productArity = fixed.length
    def hasSeq       = repeated.exists
    def elementType  = repeated.elementType
    def sequenceType = repeated.sequenceType
    def allTypes     = fixed ::: repeated.sequenceList
    def varargsTypes = fixed ::: repeated.repeatedList
    def isErroneous  = allTypes contains NoType

    private def typeStrings = fixed.map("" + _) ::: ( if (hasSeq) List("" + repeated) else Nil )

    def offeringString = if (isErroneous) "<error>" else typeStrings match {
      case Nil       => "Boolean"
      case tp :: Nil => tp
      case tps       => tps.mkString("(", ", ", ")")
    }
    override def toString = "%s => %s".format(whole, offeringString)
  }

  final case class TypedPat(pat: Pattern, tpe: Type) {
    override def toString = s"$pat: $tpe"
  }

  /** If elementArity is...
   *    0: A perfect match between extractor and the fixed patterns.
   *       If there is a star pattern it will match any sequence.
   *  > 0: There are more patterns than products. There will have to be a
   *       sequence which can populate at least <elementArity> patterns.
   *  < 0: There are more products than patterns: compile time error.
   */
  final case class Aligned(patterns: Patterns, extractor: Extractor) {
    def elementArity = patterns.nonStarArity - productArity
    def productArity = extractor.productArity
    def starArity    = patterns.starArity
    def totalArity   = patterns.totalArity

    def wholeType            = extractor.whole
    def sequenceType         = extractor.sequenceType
    def productTypes         = extractor.fixed
    def extractedTypes       = extractor.allTypes
    def typedNonStarPatterns = products ::: elements
    def typedPatterns        = typedNonStarPatterns ::: stars

    def isBool   = !isSeq && productArity == 0
    def isSingle = !isSeq && totalArity == 1
    def isStar   = patterns.hasStar
    def isSeq    = extractor.hasSeq

    private def typedAsElement(pat: Pattern)  = TypedPat(pat, extractor.elementType)
    private def typedAsSequence(pat: Pattern) = TypedPat(pat, extractor.sequenceType)
    private def productPats = patterns.fixed take productArity
    private def elementPats = patterns.fixed drop productArity
    private def products    = (productPats, productTypes).zipped map TypedPat
    private def elements    = elementPats map typedAsElement
    private def stars       = patterns.starPatterns map typedAsSequence

    override def toString = s"""
      |Aligned {
      |   patterns  $patterns
      |  extractor  $extractor
      |    arities  $productArity/$elementArity/$starArity  // product/element/star
      |      typed  ${typedPatterns mkString ", "}
      |}""".stripMargin.trim
  }
}
