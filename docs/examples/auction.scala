package examples

import java.util.Date
import scala.concurrent._

/** A simple demonstrator program implementing an online auction service
 *  The example uses the actor abstraction defined in the API of
 *  package scala.concurrent.
 */
trait AuctionMessage
case class Offer(bid: int, client: Actor) extends AuctionMessage // make a bid
case class Inquire(client: Actor) extends AuctionMessage         // inquire status

trait AuctionReply
case class Status(asked: int, expiration: Date)           // asked sum, expiration date
  extends AuctionReply
case class BestOffer() extends AuctionReply               // yours is the best offer
case class BeatenOffer(maxBid: int) extends AuctionReply  // offer beaten by maxBid
case class AuctionConcluded(seller: Actor, client: Actor) // auction concluded
  extends AuctionReply
case class AuctionFailed() extends AuctionReply           // failed with no bids
case class AuctionOver() extends AuctionReply             // bidding is closed

class Auction(seller: Actor, minBid: int, closing: Date) extends Actor {

  val timeToShutdown = 3600000 // msec
  val bidIncrement = 10

  override def run() = {
    var maxBid = minBid - bidIncrement
    var maxBidder: Actor = null
    var running = true

    while (running) {
      receiveWithin (closing.getTime() - new Date().getTime()) {

        case Offer(bid, client) =>
          if (bid >= maxBid + bidIncrement) {
            if (maxBid >= minBid)
              maxBidder send BeatenOffer(bid);
            maxBid = bid
            maxBidder = client;
            client send BestOffer()
          }
          else
            client send BeatenOffer(maxBid)

        case Inquire(client) =>
          client send Status(maxBid, closing)

        case TIMEOUT =>
          if (maxBid >= minBid) {
            val reply = AuctionConcluded(seller, maxBidder)
            maxBidder send reply
            seller send reply
          }
          else
            seller send AuctionFailed()
          receiveWithin(timeToShutdown) {
            case Offer(_, client) => client send AuctionOver()
            case TIMEOUT => running = false
          }

      }
    }
  }
}

// ---- Test -------------------------------------------------------------

object auction {

  val random = new java.util.Random()

  val minBid = 100
  val closing = new Date(new Date().getTime() + 60000)

  val seller = new Actor {
    override def run() = {}
  }
  val auction = new Auction(seller, minBid, closing)

  def client(i: int, increment: int, top: int) = new Actor {
    val name = "Client " + i
    def log(msg: String) = Console.println(name + ": " + msg)
    var running = true
    var max: int = _
    var current: int = 0
    override def run() = {
      log("started")
      auction send Inquire(this)
      receive {
        case Status(maxBid, _) => {
          log("status(" + maxBid + ")")
          max = maxBid
        }
      }
      while (running)  {
        if (max >= top)
          log("too high for me")
        else if (current < max) {
          current = max + increment
          Thread.sleep(1 + random.nextInt(1000))
          auction send Offer(current, this)
        }
        receive {
          case BestOffer() => {
            log("bestOffer(" + current + ")")
          }
          case BeatenOffer(maxBid) => {
            log("beatenOffer(" + maxBid + ")")
            max = maxBid
          }
          case AuctionConcluded(seller, maxBidder) => {
            log("auctionConcluded")
          }
          case AuctionOver() => {
            running = false
            log("auctionOver")
          }
        }
      }
    }
  }

  def kill(delay: Int) = new java.util.Timer().schedule(
    new java.util.TimerTask {
      override def run() = {
        Console.println("[killed]")
        System.exit(0)
      }
    },
    delay) // in milliseconds

  def main(args: Array[String]) = {
    seller.start()
    auction.start()
    client(1, 20, 200).start()
    client(2, 10, 300).start()
    kill(20000)
  }

}
