/* NSC -- new Scala compiler
 * Copyright 2007-2009 LAMP/EPFL
 * @author  Sean McDirmid
 */
// $Id$

package scala.tools.nsc
package doc

/**
 *  @author Stephane Micheloud, Sean McDirmid, Geoffrey Washburn
 *  @version 1.0
 */
class ModelAdditions(val global: Global) {
  import global._
  import definitions._
  def addition(sym: global.Symbol) {}

  addition(NothingClass);
  comments(NothingClass) = """
    <p>
      Class <code>Nothing</code> is - together with class <a href="Null.html">
      <code>Null</code></a> - at the bottom of the
      <a href="http://scala-lang.org" target="_top">Scala</a> type
      hierarchy.
    </p>
    <p>
      Type <code>Nothing</code> is a subtype of every other type
      (including <a href="Null.html"><code>Null</code></a>); there
      exist <em>no instances</em> of this type. Even though type
      <code>Nothing</code> is empty, it is nevertheless useful as a
      type parameter. For instance, the <a href="http://scala-lang.org"
      target="_top">Scala</a> library defines a value
      <a href="Nil$object.html"><code>Nil</code></a> of type
      <code><a href="List.html">List</a>[Nothing]</code>. Because lists
      are covariant in <a href="http://scala-lang.org" target="_top">Scala</a>,
      this makes <a href="Nil$object.html"><code>Nil</code></a> an
      instance of <code><a href="List.html">List</a>[T]</code>, for
      any element type <code>T</code>.
    </p>"""

   addition(NullClass);
   comments(NullClass) = """
     <p>
       Class <code>Null</code> is - together with class <a href="Nothing.html">
       <code>Nothing</code> - at the bottom of the
       <a href="http://scala-lang.org" target="_top">Scala</a> type
       hierarchy.
     </p>
     <p>
       Type <code>Null</code> is a subtype of all reference types; its
       only instance is the <code>null</code> reference.
       Since <code>Null</code> is not a subtype of value types,
       <code>null</code> is not a member of any such type. For instance,
       it is not possible to assign <code>null</code> to a variable of
       type <a href="Int.html"><code>Int</code></a>.
     </p>"""

   /*******************************************************************/
   /* Documentation for Any */

   addition(AnyClass);
   comments(AnyClass) = """
     <p>
       Class <code>Any</code> is the root of the <a
       href="http://scala-lang.org/"
       target="_top">Scala</a> class hierarchy. Every class in a
       <a href="http://scala-lang.org/" target="_top">Scala</a> execution
       environment inherits directly or indirectly from this class.
       Class <code>Any</code> has two direct subclasses:
       <a href="AnyRef.html"><code>AnyRef</code></a> and
       <a href="AnyVal.html"><code>AnyVal</code></a>.
     </p>"""

  addition(Any_equals);
  comments(Any_equals) = """
    This method is used to compare the receiver object (<code>this</code>)
    with the argument object (<code>arg0</code>) for equivalence.

    <p>
    The default implementations of this method is an <a
    href="http://en.wikipedia.org/wiki/Equivalence_relation">equivalence
    relation</a>:
      <ul>
      <li>It is reflexive: for any instance <code>x</code> of type <code>Any</code>,
      <code>x.equals(x)</code> should return <code>true</code>.</li>
      <li>It is symmetric: for any instances <code>x</code> and <code>y</code> of type
      <code>Any</code>, <code>x.equals(y)</code> should return <code>true</code> if and only
      if <code>y.equals(x)</code> returns <code>true</code>.</li>
      <li>It is transitive: for any instances
      <code>x</code>, <code>y</code>, and <code>z</code> of type <code>AnyRef</code>
      if <code>x.equals(y)</code> returns <code>true</code> and
      <code>y.equals(z)</code> returns
      <code>true</code>, then <code>x.equals(z)</code> should return <code>true</code>.</li>
    </ul>
    </p>

    <p>
    If you override this method, you should verify that
    your implementation remains an equivalence relation.
    Additionally, when overriding this method it is often necessary to
    override <code>hashCode</code> to ensure that objects that are
    "equal" (<code>o1.equals(o2)</code> returns <code>true</code>)
    hash to the same <a href="Int.html"><code>Int</code></a>
    (<code>o1.hashCode.equals(o2.hashCode)</code>).

    @param arg0 the object to compare against this object for equality.
    @return <code>true</code> if the receiver object is equivalent to the argument; <code>false</code> otherwise.
    </p>
    """

  addition(Any_==);
  comments(Any_==) = """
    <code>o == arg0</code> is the same as <code>o.equals(arg0)</code>.
    <p>
    @param arg0 the object to compare against this object for equality.
    @return <code>true</code> if the receiver object is equivalent to the argument; <code>false</code> otherwise.
    </p>
    """

  addition(Any_!=);
  comments(Any_!=) = """
    <code>o != arg0</code> is the same as <code>!(o == (arg0))</code>.
    <p>
    @param arg0 the object to compare against this object for dis-equality.
    @return <code>false</code> if the receiver object is equivalent to the argument; <code>true</code> otherwise.
    </p>
    """

  addition(Any_toString);
  comments(Any_toString) = """
    Returns a string representation of the object.
    <p>
    The default representation is platform dependent.

    @return a string representation of the object.
    </p>
    """

  addition(Any_asInstanceOf);
  comments(Any_asInstanceOf) = """
    This method is used to cast the receiver object to be of type <code>T0</code>.

    <p>Note that the success of a cast at runtime is modulo Scala's
    erasure semantics.  Therefore the expression
    <code>1.asInstanceOf[String]</code> will throw a
    <code>ClassCastException</code> at runtime, while the expression
    <code>List(1).asInstanceOf[List[String]]</code> will not.  In the
    latter example, because the type argument is erased as part of
    compilation it is not possible to check whether the contents of
    the list are of the requested typed.

    @throws ClassCastException if the receiver object is not an
    instance of erasure of type <code>T0</code>.
    @return the receiver object.
    </p> """

  addition(Any_isInstanceOf);
  comments(Any_isInstanceOf) = """
    This method is used to test whether the dynamic type of the receiver object is <code>T0</code>.

    <p>Note that the test result of the test is modulo Scala's erasure
    semantics.  Therefore the expression
    <code>1.isInstanceOf[String]</code> will return
    <code>false</code>, while the expression
    <code>List(1).isInstanceOf[List[String]]</code> will return
    <code>true</code>.  In the latter example, because the type
    argument is erased as part of compilation it is not possible to
    check whether the contents of the list are of the requested typed.

    @return <code>true</code> if the receiver object is an
    instance of erasure of type <code>T0</code>; <code>false</code> otherwise.
    """

  addition(Any_hashCode);
  comments(Any_hashCode) = """
    Returns a hash code value for the object.

    <p>
    The default hashing algorithm is platform dependent.

    Note that it is allowed for two objects to have identical hash
    codes (<code>o1.hashCode.equals(o2.hashCode)</code>) yet not be
    equal (<code>o1.equals(o2)</code> returns <code>false</code>).  A
    degenerate implementation could always return <code>0</code>.
    However, it is required that if two objects are equal
    (<code>o1.equals(o2)</code> returns <code>true</code>) that they
    have identical hash codes
    (<code>o1.hashCode.equals(o2.hashCode)</code>).  Therefore, when
    overriding this method, be sure to verify that the behavior is
    consistent with the <code>equals</code> method.
    </p>

    <p>
    @return the hash code value for the object.
    </p> """

   /*******************************************************************/
   /* Documentation for AnyRef */

   addition(AnyRefClass);
   comments(AnyRefClass) = """
     <p>
       Class <code>AnyRef</code> is the root class of all
       <em>reference types</em>.
     </p>"""

  addition(Object_==);
  comments(Object_==) = """
    <code>o == arg0</code> is the same as <code>if (o eq null) arg0 eq null else o.equals(arg0)</code>.
    <p>
    @param arg0 the object to compare against this object for equality.
    @return <code>true</code> if the receiver object is equivalent to the argument; <code>false</code> otherwise.
    </p>
    """

  addition(Object_ne);
  comments(Object_ne) = """
    <code>o.ne(arg0)</code> is the same as <code>!(o.eq(arg0))</code>.
    <p>
    @param arg0 the object to compare against this object for reference dis-equality.
    @return <code>false</code> if the argument is not a reference to the receiver object; <code>true</code> otherwise.
    </p>
    """


  addition(Object_finalize);
  comments(Object_finalize) = """
    This method is called by the garbage collector on the receiver object when garbage
    collection determines that there are no more references to the object.
    <p>
    The details of when and if the <code>finalize</code> method are
    invoked, as well as the interaction between <code>finalize</code>
    and non-local returns and exceptions, are all platform dependent.
    </p>
    """

  addition(Object_clone);
  comments(Object_clone) = """
    This method creates and returns a copy of the receiver object.

    <p>
    The default implementation of the <code>clone</code> method is platform dependent.

    @return a copy of the receiver object.
    </p>
    """

  addition(Object_getClass);
  comments(Object_getClass) = """
    Returns a representation that corresponds to the dynamic class of the receiver object.

    <p>
    The nature of the representation is platform dependent.

    @return a representation that corresponds to the dynamic class of the receiver object.
    </p>
    """

  addition(Object_notify);
  comments(Object_notify) = """
    Wakes up a single thread that is waiting on the receiver object's monitor.
    """

  addition(Object_notifyAll);
  comments(Object_notifyAll) = """
    Wakes up all threads that are waiting on the receiver object's monitor.
    """

  addition(Object_eq);
  comments(Object_eq) = """
    This method is used to test whether the argument (<code>arg0</code>) is a reference to the
    receiver object (<code>this</code>).

    <p>
   The <code>eq</code> method implements an
   <a href="http://en.wikipedia.org/wiki/Equivalence_relation">equivalence relation</a> on non-null instances of
   <code>AnyRef</code>:
    <ul>
    <li>It is reflexive: for any non-null instance <code>x</code> of type <code>AnyRef</code>,
    <code>x.eq(x)</code> returns <code>true</code>.</li>
    <li>It is symmetric: for any non-null instances <code>x</code> and <code>y</code> of type
    <code>AnyRef</code>, <code>x.eq(y)</code> returns <code>true</code> if and only
    if <code>y.eq(x)</code> returns <code>true</code>.</li>
    <li>It is transitive: for any non-null instances
   <code>x</code>, <code>y</code>, and <code>z</code> of type <code>AnyRef</code>
   if <code>x.eq(y)</code> returns <code>true</code> and
   <code>y.eq(z)</code> returns
   <code>true</code>, then <code>x.eq(z)</code> returns <code>true</code>.</li>
   </ul>
   Additionally, the <code>eq</code> method has three other properties.
   <ul>
     <li>It is consistent: for any non-null instances <code>x</code> and <code>y</code> of type <code>AnyRef</code>,
       multiple invocations of <code>x.eq(y)</code> consistently returns <code>true</code>
       or consistently returns <code>false</code>.</li>
     <li>For any non-null instance <code>x</code> of type <code>AnyRef</code>,
      <code>x.eq(null)</code> and <code>null.eq(x)</code> returns <code>false</code>.</li>
     <li><code>null.eq(null)</code> returns <code>true</code>.</li>
   </ul>
   </p>

    <p> When overriding the <code>equals</code> or
    <code>hashCode</code> methods, it is important to ensure that
    their behavior is consistent with reference equality.  Therefore,
    if two objects are references to each other (<code>o1 eq
    o2</code>), they should be equal to each other (<code>o1 ==
    o2</code>) and they should hash to the same value
    (<code>o1.hashCode == o2.hashCode</code>).</p>

    @param arg0 the object to compare against this object for reference equality.
    @return <code>true</code> if the argument is a reference to the receiver object; <code>false</code> otherwise.
    </p>
    """

   /*******************************************************************/

   addition(AnyValClass);
   comments(AnyValClass) = """
     <p>
       Class <code>AnyVal</code> is the root class of all
       <em>value types</em>.
     </p>
     <p>
       <code>AnyVal</code> has a fixed number subclasses, which
       describe values which are not implemented as objects in the
       underlying host system.
     </p>
     <p>
       Classes <a href="Double.html"><code>Double</code></a>,
       <a href="Float.html"><code>Float</code></a>,
       <a href="Long.html"><code>Long</code></a>,
       <a href="Int.html"><code>Int</code></a>,
       <a href="Char.html"><code>Char</code></a>,
       <a href="Short.html"><code>Short</code></a>, and
       <a href="Byte.html"><code>Byte</code></a> are together called
       <em>numeric value types</em>.
       Classes <a href="Byte.html"><code>Byte</code></a>,
       <a href="Short.html"><code>Short</code></a>, or
       <a href="Char.html"><code>Char</code></a>
       are called <em>subrange types</em>. Subrange types, as well as
       <a href="Int.html"><code>Int</code></a> and
       <a href="Long.html"><code>Long</code></a> are called
       <em>integer types</em>, whereas
       <a href="Float.html"><code>Float</code></a> and
       <a href="Double.html"><code>Double</code></a> are called
       <em>floating point types</em>.
     </p>"""

   addition(BooleanClass)
   comments(BooleanClass) = """
    <p>
      Class <code>Boolean</code> has only two values: <code>true</code>
      and <code>false</code>.
    </p>"""

   def numericValDescr(sym: Symbol) = {
     val maxValue = "MAX_" + sym.name.toString().toUpperCase()
     val minValue = "MIN_" + sym.name.toString().toUpperCase()
     addition(sym)
     comments(sym) = """
       <p>
         Class <code>""" + sym.name + """</code> belongs to the value
         classes whose instances are not represented as objects by the
         underlying host system.  There is an implicit conversion from
         instances of <code>""" + sym.name + """</code> to instances of
         <a href="runtime/Rich""" + sym.name + """.html"><code>runtime.Rich""" + sym.name + """</code></a> which
         provides useful non-primitive operations.  All value classes inherit
         from class <a href="AnyVal.html"><code>AnyVal</code></a>.
       </p>
       <p>
         Values <code>""" + maxValue + """</code> and <code>""" + minValue + """</code>
         are in defined in object <a href="Math$object.html">scala.Math</a>.
       </p>"""
   }
   (ByteClass :: CharClass :: DoubleClass :: LongClass ::
    FloatClass :: IntClass :: ShortClass :: Nil).foreach(numericValDescr);

   addition(UnitClass);
   comments(UnitClass) = """
     <p>
       Class <code>Unit</code> has only one value: <code>()</code>.
     </p>"""

   addition(UnitClass);
/*
   def boxedValDescr(what: String) = {
     val sym = definitions.getClass("java.lang." + what)
     addition(sym)
     comments(sym) = """
       <p>
         Class <code>""" + sym.name + """</code> implements the
         boxing/unboxing from/to value types.
       </p>
       <p>
         Boxing and unboxing enable value types to be treated as objects;
         they provide a unified view of the type system wherein a value
         of any type can ultimately be treated as an object.
       </p>"""
   };
   //("Float" :: "Long" :: "Number" :: "Integer" :: Nil).foreach(boxedValDescr);
*/
   object exceptions extends collection.JavaConversions.JMapWrapper[String,(Symbol,String)](
     new java.util.TreeMap()) {
     def f(name: String) {
       this("Predef." + name) = (definitions.PredefModule, name)
     }
     f("IndexOutOfBoundsException")
     f("NoSuchElementException")
     f("NullPointerException")
     f("UnsupportedOperationException")
   }
}
