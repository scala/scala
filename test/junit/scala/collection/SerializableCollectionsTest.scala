package scala.collection

import java.io.{File, ObjectStreamClass, Serializable}
import java.lang.reflect.Modifier
import java.util.jar.JarFile

import org.junit.Assert._
import org.junit.Test

import scala.jdk.CollectionConverters._

// Checks how every collection class in the library serializes, including private and anonymous
// ones, which a test cannot reach by calling the public API. Round-trip tests in
// immutable/SerializationTest and mutable/SerializationTest check that serialization also works.
class SerializableCollectionsTest {
  import SerializableCollectionsTest._

  // Class name -> why it is not serializable.
  val notSerializable = immutable.Map(
    "scala.collection.immutable.ListMap$$anon$1" -> "temporary map inside ListMap.hashCode",
    "scala.runtime.ZippedIterable2$$anon$1"      -> "deprecated with Tuple2Zipped",
    "scala.runtime.ZippedIterable3$$anon$1"      -> "deprecated with Tuple3Zipped",
    "scala.sys.SystemProperties"                 -> "live view of the JVM's properties",
  )

  // A collection should serialize through a writeReplace, usually from DefaultSerializable, so its
  // serialized form doesn't depend on implementation details. Reason -> classes that are instead
  // serialized by their fields, together with their subclasses.
  val serializedByFields = List(
    "views are serialized with their underlying collections and functions" -> List(
      "scala.collection.View",
    ),
    "wrappers are serialized with what they wrap" -> List(
      "scala.collection.convert.JavaCollectionWrappers$JCollectionWrapper",
      "scala.collection.convert.JavaCollectionWrappers$JIterableWrapper",
      "scala.collection.convert.JavaCollectionWrappers$JListWrapper",
      "scala.collection.convert.JavaCollectionWrappers$JSetWrapper",
      "scala.collection.convert.JavaCollectionWrappers$AbstractJMapWrapper",
      "scala.collection.convert.JavaCollectionWrappers$JDictionaryWrapper",
      "scala.collection.convert.JavaCollectionWrappers$JPropertiesWrapper",
      "scala.collection.immutable.ArraySeq",
      "scala.collection.mutable.ArraySeq",
      "scala.collection.immutable.WrappedString",
      "scala.collection.mutable.StringBuilder",
      "scala.collection.immutable.Map$WithDefault",
      "scala.collection.mutable.Map$WithDefault",
    ),
    "an Enumeration's ValueSet is serialized with its Enumeration and value ids" -> List(
      "scala.Enumeration$ValueSet",
    ),
    "small collections and ranges have compact forms, see SerializationStabilityTest" -> List(
      "scala.collection.immutable.Map$Map1",
      "scala.collection.immutable.Map$Map2",
      "scala.collection.immutable.Map$Map3",
      "scala.collection.immutable.Map$Map4",
      "scala.collection.immutable.Set$Set1",
      "scala.collection.immutable.Set$Set2",
      "scala.collection.immutable.Set$Set3",
      "scala.collection.immutable.Set$Set4",
      "scala.collection.immutable.SeqMap$SeqMap1",
      "scala.collection.immutable.SeqMap$SeqMap2",
      "scala.collection.immutable.SeqMap$SeqMap3",
      "scala.collection.immutable.SeqMap$SeqMap4",
      "scala.collection.immutable.Range",
      "scala.collection.immutable.NumericRange",
    ),
  )

  /** Anonymous classes serialized by their fields, with the serialVersionUID the JVM computes for
   *  them, unchanged since 2.13.1. An anonymous class cannot declare one (scala/bug#3889), and naming
   *  it would break existing data, which records the class name. The computed ID changes with nearly
   *  any change to the class, including its number among the anonymous classes of its enclosing class.
   *
   *  If this check fails, try to avoid the change. Otherwise make the class a named class with
   *  `@SerialVersionUID(3L)` and remove it here: just updating the ID breaks existing data the same
   *  way, and leaves the class as fragile.
   */
  val computedSerialVersionUID = immutable.Map(
    "scala.collection.MapView$$anon$1" -> -7674958395488560955L, // MapView.empty
    "scala.collection.View$$anon$1"    -> -6782670295915563172L, // View.fromIteratorProvider
  )

  // Class name -> why a class serialized by its fields needs no @SerialVersionUID.
  val withoutSerialVersionUID = immutable.Map(
    "scala.collection.SeqOps$$anon$1" -> "internal to SeqOps.kmpSearch, never escapes",
    "scala.collection.SeqOps$$anon$2" -> "internal to SeqOps.kmpSearch, never escapes",
    "scala.collection.SeqOps$$anon$3" -> "internal to SeqOps.kmpSearch, never escapes",
  )

  // Classes serialized by their fields that have a field which can never be serialized.
  val unserializableField = immutable.Set.empty[String]

  @Test
  def allCollectionsSerializable(): Unit = {
    assertTrue(s"found only ${collectionClasses.size} collection classes", collectionClasses.size > 100)
    checkExceptions("are not Serializable", collectionClasses.filterNot(isSerializable), notSerializable.keySet)
  }

  @Test
  def serializedThroughWriteReplace(): Unit = {
    val allowed = serializedByFields.flatMap(_._2).map(Class.forName(_, false, loader))
    val unexpected = fieldSerialized.filterNot(c => allowed.exists(_.isAssignableFrom(c))).map(_.getName).sorted
    assertTrue(s"collection classes that are serialized by their fields, not through a writeReplace:\n${unexpected.mkString("\n")}", unexpected.isEmpty)
    val stale = allowed.filterNot(a => fieldSerialized.exists(a.isAssignableFrom)).map(_.getName).sorted
    assertTrue(s"serializedByFields entries that match no class serialized by its fields:\n${stale.mkString("\n")}", stale.isEmpty)
  }

  @Test
  def serialVersionUIDs(): Unit = {
    val undeclared = fieldSerialized.filterNot(declaresSerialVersionUID)
    checkExceptions("are serialized by their fields but have no @SerialVersionUID", undeclared,
      computedSerialVersionUID.keySet ++ withoutSerialVersionUID.keySet)
    val changed = for {
      c        <- undeclared
      expected <- computedSerialVersionUID.get(c.getName)
      actual    = ObjectStreamClass.lookup(c).getSerialVersionUID
      if actual != expected
    } yield s"${c.getName}: was $expected, now $actual"
    assertTrue(s"computed serialVersionUIDs that changed, see computedSerialVersionUID:\n${changed.mkString("\n")}", changed.isEmpty)
  }

  @Test
  def serializableFields(): Unit =
    checkExceptions("have a field that cannot be serialized", fieldSerialized.filter(c => unserializableFields(c).nonEmpty), unserializableField)
}

object SerializableCollectionsTest {
  val loader: ClassLoader = classOf[Iterable[_]].getClassLoader

  def libraryClassNames: List[String] = {
    val location = new File(classOf[Iterable[_]].getProtectionDomain.getCodeSource.getLocation.toURI)
    val files =
      if (location.isDirectory) {
        val root = location.toPath
        val paths = java.nio.file.Files.walk(root)
        try paths.iterator.asScala.map(p => root.relativize(p).toString.replace(File.separatorChar, '/')).toList
        finally paths.close()
      } else {
        val jar = new JarFile(location)
        try jar.entries.asScala.map(_.getName).toList finally jar.close()
      }
    files.filter(_.endsWith(".class")).map(_.stripSuffix(".class").replace('/', '.'))
  }

  lazy val collectionClasses: List[Class[_]] = libraryClassNames.map(Class.forName(_, false, loader)).filter { cls =>
    classOf[Iterable[_]].isAssignableFrom(cls) && !cls.isInterface && !Modifier.isAbstract(cls.getModifiers)
  }

  def isSerializable(cls: Class[_]): Boolean = classOf[Serializable].isAssignableFrom(cls)

  // Serializable classes without a writeReplace, which Java serialization writes field by field.
  lazy val fieldSerialized: List[Class[_]] = collectionClasses.filter(c => isSerializable(c) && !hasWriteReplace(c))

  // The lookup of java.io.ObjectStreamClass: the first writeReplace() found in the class or its
  // superclasses, which must return Object. A trait's method counts only through the mixin
  // forwarder in the class.
  def hasWriteReplace(cls: Class[_]): Boolean = {
    def lookup(c: Class[_]): Boolean =
      if (c == null) false
      else (try Some(c.getDeclaredMethod("writeReplace")) catch { case _: NoSuchMethodException => None }) match {
        case Some(m) =>
          val mods = m.getModifiers
          m.getReturnType == classOf[Object] && !Modifier.isStatic(mods) && !Modifier.isAbstract(mods) && (
            Modifier.isPublic(mods) || Modifier.isProtected(mods) ||
            (if (Modifier.isPrivate(mods)) c == cls else c.getPackage == cls.getPackage))
        case None => lookup(c.getSuperclass)
      }
    lookup(cls)
  }

  def declaresSerialVersionUID(cls: Class[_]): Boolean =
    cls.getDeclaredFields.exists(f => f.getName == "serialVersionUID" && Modifier.isStatic(f.getModifiers))

  // Fields that Java serialization writes and whose declared type is a final class that is not
  // Serializable, so writing a non-null value always fails.
  def unserializableFields(cls: Class[_]): List[String] = {
    def never(t: Class[_]): Boolean =
      if (t.isArray) never(t.getComponentType)
      else !t.isPrimitive && !isSerializable(t) && Modifier.isFinal(t.getModifiers)
    Iterator.iterate[Class[_]](cls)(_.getSuperclass).takeWhile(c => c != null && isSerializable(c)).flatMap { c =>
      c.getDeclaredFields.iterator.filter { f =>
        val mods = f.getModifiers
        !Modifier.isStatic(mods) && !Modifier.isTransient(mods) && never(f.getType)
      }.map(f => s"${c.getName}.${f.getName}")
    }.toList
  }

  def checkExceptions(what: String, violations: List[Class[_]], exceptions: immutable.Set[String]): Unit = {
    val names = violations.map(_.getName).toSet
    val unexpected = (names -- exceptions).toList.sorted
    assertTrue(s"collection classes that $what:\n${unexpected.mkString("\n")}", unexpected.isEmpty)
    val stale = (exceptions -- names).toList.sorted
    assertTrue(s"exceptions that no longer apply (class removed, renamed or fixed):\n${stale.mkString("\n")}", stale.isEmpty)
  }
}
