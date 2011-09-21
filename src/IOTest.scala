import actor._
import impls._
import io._
import execution._
import annotation._

object IOTest extends MainActor {

  /*
  object Console extends ActorImplementor {
    val reader = new ActorImpl[Iteratee[Byte]] {
      @tailrec override def processMessage(i: Iteratee[Byte]) = {
        val its = subscribers.get
        val nits = i :: its
        if (!subscribers.compareAndSet(its, nits)) processMessage(i)
      }
    }

    private val subscribers = new java.util.concurrent.atomic.AtomicReference[List[Iteratee[Byte]]](Nil)

    start
    def start = Thread {
      while (true) {
        val in = System.in.read
        val b = if (in == -1) EOF else Data(in.toByte)

        val dones = subscribers.get.filter { it =>
          val r = it(b)
          handleAction(r.msgs)
          r.isDone
        }
        removeSubs(dones)
      }
    }
    @tailrec private def removeSubs(dones: List[Iteratee[Byte]]) {
      val its = subscribers.get
      val nits = its.filterNot(dones.contains)
      if (!subscribers.compareAndSet(its, nits)) removeSubs(dones)
    }
  }
*/

  def worder = {
    def string(chars: List[Char]) = new String(chars.reverse.toArray)
    def wf(buffer: List[Char])(in: Input[Char]): Iteratee[Char, String] = in match {
      case Data(' ') =>
        if (buffer.isEmpty) wf(Nil) _
        else cont(wf(Nil), string(buffer))
      case Data(char) => wf(char :: buffer) _
      case Empty => wf(buffer) _
      case EOF => done(string(buffer))
    }
    
    cont(wf(Nil))
  }

  def charsetDecoder(charset: String) = {
    def decode(charset: String)(buffer: Array[Byte], len: Int)(in: Input[Byte]): Iteratee[Byte, Char] = in match {
      case Data(byte) =>
        val nl = len + 1
        val b = if (nl >= buffer.length) new Array[Byte](buffer.length * 2) else buffer
        buffer(len) = byte
        val s = new String(buffer, 0, len, charset)
        if (s.length > 0) {
          val char = s.charAt(0)
          val f = decode(charset)(buffer, 0) _
          cont(f, char)
        } else decode(charset)(buffer, len) _
      case Empty => decode(charset)(buffer, len) _
      case EOF =>
        done
    }

    cont(decode(charset)(new Array(10), 0))
  }

  override def body(args: Array[String]) = {
    Noop

  }

}