package examples

import java.io._
import java.net.{InetAddress,ServerSocket,Socket,SocketException}
import java.util.Random

/**
 * Simple client/server application using Java sockets.
 *
 * The server simply generates random integer values and
 * the clients provide a filter function to the server
 * to get only values they interested in (eg. even or
 * odd values, and so on).
 */
object randomclient {

  def main(args: Array[String]) {
    val filter/*?*/ = try {
      Integer.parseInt(args(0)/*?*/) match {
        case 1 => x: Int => x % 2 != 0
        case 2 => x: Int => x % 2 == 0
        case _ => x: Int => x != 0
      }
    }
    catch {
      case _/*?*/ => x: Int => x < 100
    }

    try {
      val ia = InetAddress.getByName("localhost")
      val socket = new Socket(ia, 9999)
      val out = new ObjectOutputStream(
        new DataOutputStream(socket.getOutputStream()))
      val in = new DataInputStream(socket.getInputStream())

      out.writeObject(filter)
      out.flush()

      while (true) {
        val x = in.readInt()
        println("x = " + x)
      }
      out.close()
      in.close()
      socket.close()
    }
    catch {
      case e: IOException =>
        e.printStackTrace()
    }
  }

}

object randomserver {

  def main(args: Array[String]): Unit = {
    try {
      val listener = new ServerSocket(9999);
      while (true)
        new ServerThread(listener.accept()).start();
      listener.close()
    }
    catch {
      case e: IOException =>
        System.err.println("Could not listen on port: 9999.");
        System.exit(-1)
    }
  }

}

case class ServerThread(socket: Socket) extends Thread("ServerThread") {

  override def run(): Unit = {
    val rand = new Random(System.currentTimeMillis());
    try {
      val out = new DataOutputStream(socket.getOutputStream());
      val in = new ObjectInputStream(
        new DataInputStream(socket.getInputStream()));

      val filter = in.readObject().asInstanceOf[Int => Boolean];

      while (true) {
        var succeeded = false;
        do {
          val x = rand.nextInt(1000);
          succeeded = filter(x);
          if (succeeded) out.writeInt(x)
        } while (! succeeded);
        Thread.sleep(100)
      }

      out.close();
      in.close();
      socket.close()
    }
    catch {
      case e: SocketException =>
        () // avoid stack trace when stopping a client with Ctrl-C
      case e: IOException =>
        e.printStackTrace();
    }
  }

}