package akka.remoteinterface

/**
 * Copyright (C) 2009-2011 Scalable Solutions AB <http://scalablesolutions.se>
 */

import akka.actor.Actor
import akka.event.EventHandler

/**
 * Remote client and server event listener that pipes the events to the standard Akka EventHander.
 *
 * @author <a href="http://jonasboner.com">Jonas Bon&#233;r</a>
 */
class RemoteEventHandler extends Actor {
  import EventHandler._

  self.id = ID
  self.dispatcher = EventHandlerDispatcher

  def receive = {

    // client
    case RemoteClientError(cause, client, address) => EventHandler.error(cause, client, "RemoteClientError - Address[%s]" format address.toString)
    case RemoteClientWriteFailed(request, cause, client, address) => EventHandler.error(cause, client, "RemoteClientWriteFailed - Request[%s] Address[%s]".format(address.toString))
    case RemoteClientDisconnected(client, address) => EventHandler.info(client, "RemoteClientDisconnected - Address[%s]" format address.toString)
    case RemoteClientConnected(client, address) => EventHandler.info(client, "RemoteClientConnected - Address[%s]" format address.toString)
    case RemoteClientStarted(client, address) => EventHandler.info(client, "RemoteClientStarted - Address[%s]" format address.toString)
    case RemoteClientShutdown(client, address) => EventHandler.info(client, "RemoteClientShutdown - Address[%s]" format address.toString)

    // server
    case RemoteServerError(cause, server) => EventHandler.error(cause, server, "RemoteServerError")
    case RemoteServerWriteFailed(request, cause, server, clientAddress) => EventHandler.error(cause, server, "RemoteServerWriteFailed - Request[%s] Address[%s]" format (request, clientAddress.toString))
    case RemoteServerStarted(server) => EventHandler.info(server, "RemoteServerStarted")
    case RemoteServerShutdown(server) => EventHandler.info(server, "RemoteServerShutdown")
    case RemoteServerClientConnected(server, clientAddress) => EventHandler.info(server, "RemoteServerClientConnected - Address[%s]" format clientAddress.toString)
    case RemoteServerClientDisconnected(server, clientAddress) => EventHandler.info(server, "RemoteServerClientDisconnected - Address[%s]" format clientAddress.toString)
    case RemoteServerClientClosed(server, clientAddress) => EventHandler.info(server, "RemoteServerClientClosed - Address[%s]" format clientAddress.toString)

    case _ => //ignore other
  }
}

