/* NSC -- new Scala compiler
 * Copyright 2007-2008 LAMP/EPFL
 * @author  Sean McDirmid
 */
// $Id$

package scala.tools.nsc.doc

/**
 *  @author Stephane Micheloud, Sean McDirmid
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

  addition(Any_==);
  comments(Any_==) = """
    <p>
    <code> o1 == o2</code> is the same as <code>o1.equals(o2)</code>.
    </p>
    """

  addition(Any_!=);
  comments(Any_!=) = """
    <p>
    <code> o1 != o2</code> is the same as <code>!(o1.equals(o2))</code>.
    </p>
    """

  addition(Any_toString);
  comments(Any_toString) = """
    <p>
    Returns a string representation of the object.
    </p>
    """

  addition(Any_hashCode);
  comments(Any_hashCode) = """
    <p>
    Returns a hash code value for the object.
    </p>
    """

   addition(AnyRefClass);
   comments(AnyRefClass) = """
     <p>
       Class <code>AnyRef</code> is the root class of all
       <em>reference types</em>.
     </p>"""

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
   object exceptions extends collection.jcl.TreeMap[String,(Symbol,String)] {
     def f(name: String) {
       this("Predef." + name) = (definitions.PredefModule, name)
     }
     f("IndexOutOfBoundsException")
     f("NoSuchElementException")
     f("NullPointerException")
     f("UnsupportedOperationException")
   }
}
