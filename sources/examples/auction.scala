package examples;

import java.util.Date;
import scala.concurrent._;

trait AuctionMessage;
case class
  Offer(bid: Int, client: Actor),                     // make a bid
  Inquire(client: Actor) extends AuctionMessage;      // inquire status

trait AuctionReply;
case class
  Status(asked: Int, expiration: Date),               // asked sum, expiration date
  BestOffer(),                                        // yours is the best offer
  BeatenOffer(maxBid: Int),                           // offer beaten by maxBid
  AuctionConcluded(seller: Actor, client: Actor),     // auction concluded
  AuctionFailed(),                                    // failed with no bids
  AuctionOver() extends AuctionReply;                 // bidding is closed

class Auction(seller: Actor, minBid: Int, closing: Date) extends Actor() {

  val timeToShutdown = 36000000; // msec
  val bidIncrement = 10;
  override def run() = execute;
  def execute = {
    var maxBid = minBid - bidIncrement;
    var maxBidder: Actor = _;
    var running = true;
    while (running) {
      receiveWithin ((closing.getTime() - new Date().getTime())) {
	case Offer(bid, client) =>
	  if (bid >= maxBid + bidIncrement) {
            if (maxBid >= minBid) maxBidder send BeatenOffer(bid);
            maxBid = bid;
            maxBidder = client;
            client send BestOffer();
          } else {
            client send BeatenOffer(maxBid);
          }

	case Inquire(client) =>
	  client send Status(maxBid, closing);

	case TIMEOUT() =>
	  if (maxBid >= minBid) {
	    val reply = AuctionConcluded(seller, maxBidder);
	    maxBidder send reply;
	    seller send reply;
	  } else {
	    seller send AuctionFailed();
          }
          receiveWithin(timeToShutdown) {
            case Offer(_, client) => client send AuctionOver()
            case TIMEOUT() => running = false;
          }
      }
    }
  }
}
