package client



object Client extends App {
  val peer = actors.remote.Node("localhost", 23456)
  val a = actors.remote.RemoteActor.select(peer, 'test)
  a ! server.TestMsg
}
