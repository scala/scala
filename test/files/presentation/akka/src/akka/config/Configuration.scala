/**
 * Copyright (C) 2009-2011 Scalable Solutions AB <http://scalablesolutions.se>
 *
 * Based on Configgy by Robey Pointer.
 *   Copyright 2009 Robey Pointer <robeypointer@gmail.com>
 *   http://www.apache.org/licenses/LICENSE-2.0
 */

package akka.config

import java.io.File

object Configuration {
  val DefaultPath = new File(".").getCanonicalPath
  val DefaultImporter = new FilesystemImporter(DefaultPath)

  def load(data: String, importer: Importer = DefaultImporter): Configuration = {
    val parser = new ConfigParser(importer = importer)
    new Configuration(parser parse data)
  }

  def fromFile(filename: String, importer: Importer): Configuration = {
    load(importer.importFile(filename), importer)
  }

  def fromFile(path: String, filename: String): Configuration = {
    val importer = new FilesystemImporter(path)
    fromFile(filename, importer)
  }

  def fromFile(filename: String): Configuration = {
    val n = filename.lastIndexOf('/')
    if (n < 0) {
      fromFile(DefaultPath, filename)
    } else {
      fromFile(filename.substring(0, n), filename.substring(n + 1))
    }
  }

  def fromResource(filename: String): Configuration = {
    fromResource(filename, ClassLoader.getSystemClassLoader)
  }

  def fromResource(filename: String, classLoader: ClassLoader): Configuration = {
    val importer = new ResourceImporter(classLoader)
    fromFile(filename, importer)
  }

  def fromMap(map: Map[String, Any]) = {
    new Configuration(map)
  }

  def fromString(data: String): Configuration = {
    load(data)
  }
}

class Configuration(val map: Map[String, Any]) {
  private val trueValues = Set("true", "on")
  private val falseValues = Set("false", "off")

  def contains(key: String): Boolean = map contains key

  def keys: Iterable[String] = map.keys

  def getAny(key: String): Option[Any] = {
    try {
      Some(map(key))
    } catch {
      case _ => None
    }
  }

  def getAny(key: String, defaultValue: Any): Any = getAny(key).getOrElse(defaultValue)

  def getSeqAny(key: String): Seq[Any] = {
    try {
      map(key).asInstanceOf[Seq[Any]]
    } catch {
      case _ => Seq.empty[Any]
    }
  }

  def getString(key: String): Option[String] = map.get(key).map(_.toString)

  def getString(key: String, defaultValue: String): String = getString(key).getOrElse(defaultValue)

  def getList(key: String): Seq[String] = {
    try {
      map(key).asInstanceOf[Seq[String]]
    } catch {
      case _ => Seq.empty[String]
    }
  }

  def getInt(key: String): Option[Int] = {
    try {
      Some(map(key).toString.toInt)
    } catch {
      case _ => None
    }
  }

  def getInt(key: String, defaultValue: Int): Int = getInt(key).getOrElse(defaultValue)

  def getLong(key: String): Option[Long] = {
    try {
      Some(map(key).toString.toLong)
    } catch {
      case _ => None
    }
  }

  def getLong(key: String, defaultValue: Long): Long = getLong(key).getOrElse(defaultValue)

  def getFloat(key: String): Option[Float] = {
    try {
      Some(map(key).toString.toFloat)
    } catch {
      case _ => None
    }
  }

  def getFloat(key: String, defaultValue: Float): Float = getFloat(key).getOrElse(defaultValue)

  def getDouble(key: String): Option[Double] = {
    try {
      Some(map(key).toString.toDouble)
    } catch {
      case _ => None
    }
  }

  def getDouble(key: String, defaultValue: Double): Double = getDouble(key).getOrElse(defaultValue)

  def getBoolean(key: String): Option[Boolean] = {
    getString(key) flatMap { s =>
      val isTrue = trueValues.contains(s)
      if (!isTrue && !falseValues.contains(s)) None
      else Some(isTrue)
    }
  }

  def getBoolean(key: String, defaultValue: Boolean): Boolean = getBool(key).getOrElse(defaultValue)

  def getBool(key: String): Option[Boolean] = getBoolean(key)

  def getBool(key: String, defaultValue: Boolean): Boolean = getBoolean(key, defaultValue)

  def apply(key: String): String = getString(key) match {
    case None    => throw new ConfigurationException("undefined config: " + key)
    case Some(v) => v
  }

  def apply(key: String, defaultValue: String) = getString(key, defaultValue)
  def apply(key: String, defaultValue: Int) = getInt(key, defaultValue)
  def apply(key: String, defaultValue: Long) = getLong(key, defaultValue)
  def apply(key: String, defaultValue: Boolean) = getBool(key, defaultValue)

  def getSection(name: String): Option[Configuration] = {
    val l = name.length + 1
    val m = map.collect { case (k, v) if k.startsWith(name) => (k.substring(l), v) }
    if (m.isEmpty) None
    else Some(new Configuration(m))
  }
}
