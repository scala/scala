package server



object Server extends App {
  
  class ServerActor extends actors.Actor {
    def act() {
      actors.remote.RemoteActor.alive(23456)
      actors.remote.RemoteActor.register('test, actors.Actor.self)
      loop {
        react {
          case TestMsg => println("Yay!")
        }
      }
    }
  }
  
  val a = new ServerActor
  a.start()
  
}
