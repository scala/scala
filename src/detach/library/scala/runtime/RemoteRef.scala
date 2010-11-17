/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: RemoteRef.scala 18365 2009-07-21 11:00:42Z michelou $

package scala.runtime

import java.net.{InetAddress, MalformedURLException}
import java.rmi.{NoSuchObjectException, NotBoundException, Remote}
import java.rmi.registry.{LocateRegistry, Registry}
import java.rmi.server.{ExportException, RemoteObject, UnicastRemoteObject}

import scala.runtime.remoting.{Debug, RemoteGC}

/**
 *
 *  @author Stephane Micheloud
 *  @version 1.0
 */
object RemoteRef { /*extends Thread {
  start()

  private class QuitException extends Exception
  private var isTerminated = false

  // keeps track of live remote objects
  val remoteGC = new RemoteGC

  override def run() {
    info("started thread")
    try {
      while (!isTerminated) {
        this.synchronized {
          try {
            wait(200)
          } catch {
            case _: InterruptedException =>
              if (isTerminated) throw new QuitException
          }
          remoteGC.gc()
          if (remoteGC.allClosed)
            throw new QuitException
        } // synchronized

      }
    } catch {
      case _: QuitException =>
        // allow thread to exit
    }
  }
*/
  try {
    val prop = System.getProperty("sun.rmi.dgc.server.gcInterval")
    if (prop eq null)
      System.setProperty("sun.rmi.dgc.server.gcInterval", "10000")
  }
  catch {
    case e =>
      error(e.getMessage)
  }

  private val host =
    try {
      val prop = System.getProperty("java.rmi.server.hostname")
      if (prop ne null) prop else InetAddress.getLocalHost.getHostAddress
    }
    catch {
      case e =>
        warning(e.getMessage)
        InetAddress.getLocalHost.getHostAddress
    }

  private val port =
    try {
      val prop = System.getProperty("scala.remoting.port")
      if (prop ne null) prop.toInt else Registry.REGISTRY_PORT
    }
    catch {
      case e =>
        warning(e.getMessage)
        Registry.REGISTRY_PORT // default port
    }

  private val registry =
    try {
      LocateRegistry.createRegistry(port)
    }
    catch {
      case e =>
        warning(e.getMessage)
        LocateRegistry.getRegistry(host, port)
    }

  private val prefix = "//"+host+":"+port+"/"
  printDebugInfos

  // Variant 1: rebind/unbind
  def bind(name: String, x: Remote): Remote =
    try {
      registry.rebind(prefix+name, x)
      info("\""+prefix+name+"\" bound")
      val stub = RemoteObject.toStub(x)
      //remoteGC.newRef(stub)
      stub
    } catch {
      case e: MalformedURLException =>
        error(e.getMessage); null
      case e: ExportException =>
        info(""+e); null
      case e: Exception => // AlreadyBoundException, etc..
        throw e
    }

  def unbind(name: String) =
    try {
      registry.unbind(prefix+name)
      info("\""+name+"\" unbound")
    } catch {
      case e: java.io.EOFException =>
        warning(e.getMessage)
      case e: NotBoundException =>
        warning(e.getMessage+" already unbound")
      case e: MalformedURLException =>
        error(e.getMessage)
      case e: Exception =>
        throw e
    }
/*
  // Variant 2: un-/exportObject
  def bind(name: String, x: Remote): Remote =
    try {
      val ex = UnicastRemoteObject.exportObject(x)
      registry.rebind(prefix+name, ex)
      info("\""+prefix+name+"\" bound")
      //val stub = RemoteObject.toStub(ex)
      //remoteGC.newRef(ex)
      ex //stub
    } catch {
      case e: MalformedURLException =>
        error(e.getMessage); null
      case e: ExportException =>
        info(""+e); null
      case e: Exception => // AlreadyBoundException, etc..
        throw e
    }

  def unbind(x: Remote) {
    try {
      UnicastRemoteObject.unexportObject(x, false)
      info("\""+x+"\" unbound")
    } catch {
      case e: java.io.EOFException =>
        warning(e.getMessage)
      case e: NotBoundException =>
        warning(e.getMessage+" already unbound")
      case e: MalformedURLException =>
        error(e.getMessage)
      case e: Exception =>
        throw e
    }
  }
*/
  private def info(msg: String) { Debug.info("[RemoteRef] "+msg) }
  private def warning(msg: String) { Debug.warning("[RemoteRef] "+msg) }
  private def error(msg: String) { Debug.error("[RemoteRef] "+msg) }

  private def printDebugInfos {
    def property(name: String): String =
      name+"="+(
      try { System.getProperty(name, "") }
      catch { case e => warning(e.getMessage); "?" })
    info(property("java.rmi.server.hostname"))
    info(property("sun.rmi.dgc.server.gcInterval"))
    info("registry="+registry)
    info("prefix="+prefix)
  }
}
