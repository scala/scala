import scala.concurrent.Process._;

class D() {
 def start_listener(Child:Process) = {
    var running = true;
    while (running) {
      receiveWithin(0) {
        case TIMEOUT() => {
	    Child ! 'foo;
        }
      }
    }
  }
}
