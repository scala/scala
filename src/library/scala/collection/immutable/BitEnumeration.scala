package scala.collection.immutable;
import scala.collection.mutable;

/** Holds the two underlying bit enumeration representations.
 *  @author Sean McDirmid
 */
object BitEnumeration {
  def At32 = new At32;
  /** A bit enumeration that supports at most 32 values
   */
  class At32 extends BitEnumeration {
    type Underlying = Int;
    def maxBits = 32;
    def Set(underlying : Int) = new Set(underlying);
    class Set(val underlying : Int) extends SetImpl {
      def underlyingAsLong = {
        if (underlying >= 0) underlying.toLong;
        else {
          val underlying0 = (~(1 << 31)) & underlying;
          assert(underlying0 >= 0);
          underlying0.toLong | (1L << 31);
        }
      }
      def contains(value : Value) = (underlying & value.mask32) != 0;
      def |(  set :   Set) = new Set(underlying |   set.underlying);
      def |(value : Value) = new Set(underlying | value.mask32);
      def &~(value : Value) = new Set(underlying & (~value.mask32));
      def &(set : Set) = new Set(underlying & set.underlying);
    }
  }
  def At64 = new At64;
  /** A bit enumeration that supports at most 64 values
   */
  class At64 extends BitEnumeration {
    type Underlying = Long;
    def maxBits = 64;
    def Set(underlying : Long) = new Set(underlying);
    class Set(val underlying : Long) extends SetImpl {
      def underlyingAsLong = underlying;
      def contains(value : Value) = (underlying & value.mask64) != 0;
      def |(  set :   Set) = new Set(underlying |   set.underlying);
      def |(value : Value) = new Set(underlying | value.mask64);
      def &~(value : Value) = new Set(underlying & (~value.mask64));
      def &(set : Set) = new Set(underlying & set.underlying);
    }
  }
}

/** Defines a finite set of values specific to an enumeration. Unlike
  * <code>scala.Enumeration</code>, each value in a bit enumeration is
  * associated with a unique bit that enables efficient representation
  * and manipulation as an integer bit mask. For this reason, only a
  * small number (32 or 64) of values are supported per bit enumeration.
  * Concrete sub-classes of this class include <code>BitEnumeration.At32</code>
  * and <code>BitEnumeration.At64</code>, which respectively support 32 and 64
  * values.
  *
  * @ex
  <style type="text/css">
    p.p1 {margin: 0.0px 0.0px 0.0px 0.0px; font: 18.0px Courier New}
    p.p2 {margin: 0.0px 0.0px 0.0px 0.0px; font: 18.0px Courier New; color: #913030}
    span.s1 {color: #808080}
    span.s2 {color: #000000}
    span.s3 {color: #1a417b}
    span.s4 {color: #4c4c4c}
    span.s5 {color: #913030}
    span.s6 {color: #1229cd}
  </style>
  * <p class="p1">    <span class="s1"><b>import</b></span> <b>scala</b>.<b>collection</b>.<b>immutable</b>.<b>BitEnumeration</b>;</p>
  * <p class="p2"><span class="s2">    </span><span class="s1"><b>object</b></span><span class="s2"> </span><b>fruits</b><span class="s2"> </span><span class="s1"><b>extends</b></span><span class="s2"> </span><b>BitEnumeration</b><span class="s2">.</span><b>At32</b><span class="s2"> {</span></p>
  *
  * <p class="p1">      <span class="s1"><b>override</b></span> <span class="s1"><b>def</b></span> <span class="s3"><b>stringPrefix</b></span> = <span class="s4"><i>"fruits"</i></span>;</p>
  * <p class="p1">    }</p>
  * <p class="p2"><span class="s2">    </span><span class="s1"><b>object</b></span><span class="s2"> </span><b>Apple</b><span class="s2"> </span><span class="s1"><b>extends</b></span><span class="s2"> </span><b>fruits</b><span class="s2">.</span><b>Value</b><span class="s2">(</span><span class="s4"><i>"apple"</i></span><span class="s2">, 1);</span></p>
  * <p class="p2"><span class="s2">    </span><span class="s1"><b>object</b></span><span class="s2"> </span><b>Orange</b><span class="s2"> </span><span class="s1"><b>extends</b></span><span class="s2"> </span><b>fruits</b><span class="s2">.</span><b>Value</b><span class="s2">(</span><span class="s4"><i>"orange"</i></span><span class="s2">, 31);</span></p>
  * <p class="p1">    <span class="s1"><b>object</b></span> <span class="s5"><b>Pear</b></span> <span class="s1"><b>extends</b></span> <span class="s5"><b>fruits</b></span>.<span class="s5"><b>Value</b></span>(<span class="s4"><i>"pear"</i></span>, 15);</p>
  * <p class="p1">    </p>
  * <p class="p1">    <span class="s1"><b>val</b></span> <span class="s6"><b>set</b></span> = <span class="s5"><b>fruits</b></span>.<span class="s3"><b>Empty</b></span>;</p>
  * <p class="p1">    <span class="s1"><b>val</b></span> <span class="s6"><b>set0</b></span> = <span class="s6"><b>set</b></span> <span class="s3"><b>|</b></span> <span class="s5"><b>Apple</b></span> <span class="s3"><b>|</b></span> <span class="s5"><b>Orange</b></span> <span class="s3"><b>|</b></span> <span class="s5"><b>Pear</b></span>;</p>
  * <p class="p1">    <span class="s1"><b>val</b></span> <span class="s6"><b>set1</b></span> = <span class="s6"><b>set0</b></span> <span class="s3"><b>&amp;~</b></span> <span class="s5"><b>Orange</b></span>;</p>
  *
  * @param names Names that are added to the enumeration upon creation.
  *
  * @author  Sean McDirmid
  */
sealed abstract class BitEnumeration {
  private val bits = new mutable.HashMap[Int,Value];
  /** Maximum number of values supported by this bit enumeration. */
  protected def maxBits : Int;
  /** Create a new value of a specified <code>name</code> and <code>bit</code>.
   *  @throw IllegalArgumentException if <code>bit</code> is already reserved for another value or is greater than <code>maxBits</code>.
   */
  def Value(name : String, bit : Int) = new Value(name, bit);
  /** Create a new value of a specified <code>name</code>, choose first available bit.
   *  @throw IllegalArgumentException if all bits in enumeration are already reserved.
   */
  def Value(name : String           ) : Value = Value(name, {
    var bit = 0;
    while (bits.contains(bit)) bit = bit + 1;
    bit;
  });
  /** Create a new value of a specified <code>bit</code>, name not specified.
   *  @throw IllegalArgumentException if <code>bit</code> is already reserved for another value or is greater than <code>maxBits</code>.
   */
  def Value(               bit : Int) : Value = Value(null, bit);
   /** Create a new value with no name, choose first available bit.
    *  @throw IllegalArgumentException if all bits in enumeration are already reserved.
    */
  def Value(                        ) : Value = Value(null);

  /** Represents values of the enumeration with a <code>name</code>, which is used for printing purposes only,
   *  and a <code>bit</code> that encodes this value in a set representation.
   */
  class Value(name : String, bit : Int) {

    val existing = bits.get(bit);
    if (!existing.isEmpty || bit >= maxBits)
      throw new IllegalArgumentException("bit " + bit + " is invalid for " + this + {
        if (!existing.isEmpty) " - already used for " + existing.get;
        else "";
      });
    bits += (bit -> this);
    override def toString = {
      if (name == null) "bit" + bit else name;
    }
    def mask32 : Int  = 1  << bit;
    def mask64 : Long = 1L << bit;
  }
  /** Represents wrappers around integers that bit-encode sets of enumeration values.
   */
  type Set <: SetImpl;
  /** Underlying integer type of a set based on this bit enumeration.
   */
  type Underlying <: AnyVal;
  /** A set that contains no values in this bit enumeration.
   */
  def Empty : Set = Set(0.asInstanceOf[Underlying]);
  /** Given an integer that bit encodes a set of enumeration values,
   *  produces an object that wraps this integer with high-level Set APIs.
   */
  def Set(underlying : Underlying) : Set;
  protected def stringPrefix = "Bits";

  abstract class SetImpl extends collection.immutable.Set[Value] {
    /** The integer that bit-encodes a set of enumeration values.
     */
    val underlying : Underlying;
    /** Equivalent to <code>++</code> for bit sets. Returns a set
     *  that has all values in <code>this</code> and <code>set</code>.
     */
    def |(set   : Set  ) : Set;
    /** Equivalent to <code>+</code> for bit sets. Returns a set
     *  that has all values in <code>this</code> with the addition of <code>value</code>.
     */
    def |(value : Value) : Set;
    /** Equivalent to <code>**</code> for bit sets.
     *  Returns a bit set that has all values that are both in <code>this</code> and <code>set</code>.
     */
    def &(set   : Set  ) : Set;
    /** Equivalent to <code>-</code> for bit sets.
     *  Returns a bit set that has all values in <code>this</code> except for <code>value</code>.
     */
    def &~(value : Value) : Set;
    /** returns the underlying integer representation of this set as a long. */
    protected def underlyingAsLong : Long;
    def -(value : Value) : Set = this &~ value;
    def +(value : Value) : Set = this | value;
    def ++(set : Set) : Set = this | set;
    def **(set : Set) : Set = this & set;
    def size = {
      var x = underlyingAsLong;
      var sz = 0;
      while (x != 0) {
        if ((x & 1) != 0) sz = sz + 1;
        x = x >> 1;
      }
      sz;
    }
    override def stringPrefix = BitEnumeration.this.stringPrefix;
    def elements = new Iterator[Value] {
      var bit = 0;
      var underlying = underlyingAsLong;
      def hasNext = underlying != 0;
      private def shift = {
        underlying = underlying >> 1;
        bit = bit + 1;
      }
      def next = {
        if (underlying == 0) throw new NoSuchElementException;
        while ((underlying & 1) == 0) shift;
        val ret = bits(bit);
        shift;
        ret;
      }
    }
    def empty[B]: scala.collection.immutable.Set[B] = new scala.collection.immutable.HashSet[B];
  }
}
