package scala

/**
 * The base package of Scala's reflection library.
 *
 * The refection library is structured according to the 'cake pattern'. The base layer 
 * resides in package [[scala.reflect.base]] and defines an interface to the following main types:
 *   
 *   - [[scala.reflect.base.Types#Type Types]] represent types
 *   - [[scala.reflect.base.Symbol#Symbol Symbols]] represent definitions
 *   - [[scala.reflect.base.Trees#Tree Trees]] represent abstract syntax trees
 *   - [[scala.reflect.base.Names#Name Names]] represent term and type names
 *   - [[scala.reflect.base.AnnotationInfos#AnnotationInfo AnnotationInfos]] represent annotations
 *   - [[scala.reflect.base.Position#Positions Positions]] represent source positions of tree nodes
 *   - [[scala.reflect.base.FlagSets#FlagSet FlagSet]] represent sets of flags that apply to symbols and 
 *     definition trees
 *   - [[scala.reflect.base.Constansts#Constant Constants]] represent compile-time constants. 
 *
 * Each of these types are defined in their own enclosing traits, which are ultimately all inherited by class 
 * [[scala.reflect.base.Universe Universe]]. The base universe defines a minimal interface to the above types.
 * Universes that provide additional functionality such as deeper introspection or runtime code generation, 
 * are defined in packages [[scala.reflect.api]] and [[scala.tools.reflect]].
 *
 * The cake pattern employed here requires to write certain Scala idioms with more indirections that usual. 
 * What follows is a description of these indirections, which will help to navigate the Scaladocs easily.
 *
 * For instance, consider the base type of all abstract syntax trees: [[scala.reflect.base.Trees#Tree]]. 
 * This type is not a class but is abstract and has an upper bound of [[scala.reflect.base.Trees#TreeBase]], 
 * which is a class defining the minimal base interface for all trees. 
 *
 * For a more interesting tree type, consider [[scala.reflect.base.Trees#If]] representing if-expressions. 
 * It does not come with a class `IfBase`, since it does not add anything to the interface of its upper 
 * bound `TermTree`. However, it is defined next to a value `If` of type [[scala.reflect.base.Trees#IfExtractor]]. 
 * This value serves as the companion object defining a factory method `apply` and a corresponding `unapply` 
 * for pattern matching. Moreover, there is an implicit value [[scala.reflect.base.Trees#IfTag]] of type 
 * `ClassTag[If]` that is used by the Scala compiler so that we can indeed pattern match on `If`:
 * {{{
 *   tree match { case ifTree: If => ... }
 * }}} 
 * Without the given implicit value, this pattern match would raise an "unchecked" warning at compile time
 * since `If` is an abstract type that gets erased at runtime. See [[scala.reflect.ClassTag]] for details.
 *
 * To summarize: each tree type `X` (and similarly for other types such as `Type` or `Symbol`) is represented 
 * by an abstract type `X`, optionally together with a class `XBase` that defines `X`'s' interface. 
 * `X`'s companion object, if it exists, is represented by a value `X` that is of type `XExtractor`. 
 * Moreover, for each type `X`, there is a value `XTag` of type `ClassTag[X]` that allows to pattern match 
 * on `X`.
 */
package object reflect {

  lazy val basis: base.Universe = new base.Base

  // in the new scheme of things ClassManifests are aliased to ClassTags
  // this is done because we want `toArray` in collections work with ClassTags
  // but changing it to use the ClassTag context bound without aliasing ClassManifest
  // will break everyone who subclasses and overrides `toArray`
  // luckily for us, aliasing doesn't hamper backward compatibility, so it's ideal in this situation
  // I wish we could do the same for Manifests and TypeTags though

  // note, by the way, that we don't touch ClassManifest the object
  // because its Byte, Short and so on factory fields are incompatible with ClassTag's

  /** A `ClassManifest[T]` is an opaque descriptor for type `T`.
   *  It is used by the compiler to preserve information necessary
   *  for instantiating `Arrays` in those cases where the element type
   *  is unknown at compile time.
   *
   *  The type-relation operators make an effort to present a more accurate
   *  picture than can be realized with erased types, but they should not be
   *  relied upon to give correct answers. In particular they are likely to
   *  be wrong when variance is involved or when a subtype has a different
   *  number of type arguments than a supertype.
   */
  @deprecated("Use scala.reflect.ClassTag instead", "2.10.0")
  @annotation.implicitNotFound(msg = "No ClassManifest available for ${T}.")
  type ClassManifest[T]  = scala.reflect.ClassTag[T]

  /** The object `ClassManifest` defines factory methods for manifests.
   *  It is intended for use by the compiler and should not be used in client code.
   */
  @deprecated("Use scala.reflect.ClassTag instead", "2.10.0")
  val ClassManifest = ClassManifestFactory

  /** The object `Manifest` defines factory methods for manifests.
   *  It is intended for use by the compiler and should not be used in client code.
   */
  @deprecated("Use scala.reflect.ClassTag (to capture erasures), scala.reflect.runtime.universe.TypeTag (to capture types) or both instead", "2.10.0")
  val Manifest = ManifestFactory

  def classTag[T](implicit ctag: ClassTag[T]) = ctag
  // typeTag incantation is defined inside scala.reflect.basis and scala.reflect.runtime.universe

  // ClassTag class is defined in ClassTag.scala
  type TypeTag[T]        = scala.reflect.basis.TypeTag[T]

  // ClassTag object is defined in ClassTag.scala
  lazy val TypeTag       = scala.reflect.basis.TypeTag

  @deprecated("Use `@scala.beans.BeanDescription` instead", "2.10.0")
  type BeanDescription = scala.beans.BeanDescription
  @deprecated("Use `@scala.beans.BeanDisplayName` instead", "2.10.0")
  type BeanDisplayName = scala.beans.BeanDisplayName
  @deprecated("Use `@scala.beans.BeanInfo` instead", "2.10.0")
  type BeanInfo = scala.beans.BeanInfo
  @deprecated("Use `@scala.beans.BeanInfoSkip` instead", "2.10.0")
  type BeanInfoSkip = scala.beans.BeanInfoSkip
  @deprecated("Use `@scala.beans.BeanProperty` instead", "2.10.0")
  type BeanProperty = scala.beans.BeanProperty
  @deprecated("Use `@scala.beans.BooleanBeanProperty` instead", "2.10.0")
  type BooleanBeanProperty = scala.beans.BooleanBeanProperty
  @deprecated("Use `@scala.beans.ScalaBeanInfo` instead", "2.10.0")
  type ScalaBeanInfo = scala.beans.ScalaBeanInfo
}

/** An exception that indicates an error during Scala reflection */
case class ScalaReflectionException(msg: String) extends Exception(msg)
