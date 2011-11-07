package examples.pilib

/**
* Mobile phone protocol.
* Equivalent to a three-place buffer.
* @see Bjoern Victor "A verification tool for the polyadic pi-calculus".
*/
object mobilePhoneProtocol {

  import concurrent.pilib._

  val random = new java.util.Random()

  // Internal messages exchanged by the protocol.
  trait Message

  // Predefined messages used by the protocol.
  case class Data()   extends Message;
  case class HoCmd()  extends Message; // handover command
  case class HoAcc()  extends Message; // handover access
  case class HoCom()  extends Message; // handover complete
  case class HoFail() extends Message; // handover fail
  case class ChRel()  extends Message; // release
  case class Voice(s: String) extends Message; // voice 
  case class Channel(n: Chan[Message]) extends Message; // channel

  def MobileSystem(in: Chan[String], out: Chan[String]): unit = {

    def CC(fa: Chan[Message], fp: Chan[Message], l: Chan[Channel]): unit =
      choice (
        in * (v => { fa.write(Data()); fa.write(Voice(v)); CC(fa, fp, l) })
        ,
        l * (m_new => {
          fa.write(HoCmd());
          fa.write(m_new);
          choice (
            fp * ({ case HoCom() => {
              System.out.println("Mobile has moved from one cell to another");
              fa.write(ChRel());
              val Channel(m_old) = fa.read;
              l.write(Channel(m_old));
              CC(fp, fa, l)
            }})
            ,
            fa * ({ case HoFail() => {
              System.out.println("Mobile has failed to move from one cell to another");
              l.write(m_new);
              CC(fa, fp, l)
            }})
          )
        })
      );

    /*
    * Continuously orders the MSC to switch the MS to the non-used BS.
    */
    def HC(l: Chan[Channel], m: Chan[Message]): unit = {
      Thread.sleep(1 + random.nextInt(1000));
      l.write(Channel(m));
      val Channel(m_new) = l.read;
      HC(l, m_new)
    }

    /**
    * Mobile switching center.
    */
    def MSC(fa: Chan[Message], fp: Chan[Message], m: Chan[Message]): unit = {
      val l = new Chan[Channel];
      spawn < HC(l, m) | CC(fa, fp, l) >
    }

    /**
    * Active base station.
    */
    def BSa(f: Chan[Message], m: Chan[Message]): unit =
      (f.read) match {
        case Data() => {
          val v = f.read;
          m.write(Data());
          m.write(v);
          BSa(f, m)
        }
        case HoCmd() => {
          val v = f.read;
          m.write(HoCmd());
          m.write(v);
          choice (
            f * ({ case ChRel() => {
              f.write(Channel(m));
              BSp(f, m)
            }})
            ,
            m * ({ case HoFail() => {
              f.write(HoFail());
              BSa(f, m)
            }})
          )
        }
      };

    /**
     * Passive base station.
     */
    def BSp(f: Chan[Message], m: Chan[Message]): unit = {
      val HoAcc = m.read
      f.write(HoCom())
      BSa(f, m)
    }

    /**
     * Mobile station.
     */
    def MS(m: Chan[Message]): unit =
      (m.read) match {
        case Data() => {
          val Voice(v) = m.read;
          out.write(v);
          MS(m)
        }
        case HoCmd() =>
          (m.read) match {
            case Channel(m_new) => {
              if (random.nextInt(1) == 0)
                choice ( m_new(HoAcc()) * (MS(m_new)) );
              else
                choice ( m(HoFail()) * (MS(m)) );
            }
          }
      };

    def P(fa: Chan[Message], fp: Chan[Message]): unit = {
      val m = new Chan[Message];
      spawn < MSC(fa, fp, m) | BSp(fp, m) >
    }

    def Q(fa: Chan[Message]): unit = {
      val m = new Chan[Message];
      spawn < BSa(fa, m) | MS(m) >
    }

    val fa = new Chan[Message];
    val fp = new Chan[Message];
    spawn < Q(fa) | P(fa, fp) >;
  }

  //***************** Entry function ******************//
  
  def main(args: Array[String]): unit = {
    
    def Producer(n: Int, put: Chan[String]): unit = {
      Thread.sleep(1 + random.nextInt(1000));
      val msg = "object " + n;
      put.write(msg);
      System.out.println("Producer gave " + msg);
      Producer(n + 1, put)
    }
    
    def Consumer(get: Chan[String]): unit = {
      Thread.sleep(1 + random.nextInt(1000));
      val msg = get.read;
      System.out.println("Consumer took " + msg);
      Consumer(get)
    }
    
    val put = new Chan[String];
    val get = new Chan[String];
    spawn < Producer(0, put) | Consumer(get) | MobileSystem(put, get) >
  }

}


