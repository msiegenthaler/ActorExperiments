import actor._
import impls._
import io.iteratee._
import Iteratee._
import IterateeFun._
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
        if (buffer.isEmpty) cont(wf(Nil))
        else cont(wf(Nil), string(buffer))
      case Data(char) => cont(wf(char :: buffer))
      case Empty => cont(wf(buffer))
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
        val s = new String(buffer, 0, nl, charset)
        if (s.length > 0) {
          val char = s.charAt(0)
          val f = decode(charset)(buffer, 0) _
          cont(f, char)
        } else cont(decode(charset)(buffer, nl))
      case Empty => cont(decode(charset)(buffer, len))
      case EOF =>
        done
    }

    cont(decode(charset)(new Array(10), 0))
  }

  def printlnToConsole = {
    def print(in: Input[String]): Iteratee[String, Unit] = {
      in match {
        case Data(d) => println(d)
        case Empty => ()
        case EOF => ()
      }
      cont(print)
    }
    cont(print)
  }

  def endDebugIteratee[A](name: String) = {
    def handle(in: Input[A]): Iteratee[A, Unit] = {
      println("Iteratee " + name + ": " + in)
      cont(handle)
    }
    cont(handle)
  }

  def debug[A](name: String) = {
    def handle(in: Input[A]): Iteratee[A, A] = {
      println("Iteratee " + name + ": " + in)
      in match {
        case Data(d) => cont(handle, d)
        case Empty => cont(handle)
        case EOF => done
      }
    }
    cont(handle)
  }

  def iterate[E, O](l: List[E])(it: Iteratee[E, O]): Unit = {
    l match {
      case e :: t =>
        val nit = it(Data(e))
        if (nit.isDone) ()
        else iterate(t)(nit)
      case Nil =>
        it(EOF)
    }
  }

  override def body(args: Array[String]) = {
    val in = "Mario is doing some tests tonight".getBytes("UTF-8").toList
    
    val it1 = charsetDecoder("UTF-8") compose worder compose mapping(_.toUpperCase) compose printlnToConsole
    iterate(in)(it1)
    
    println("--------------")
    
    val it2 = mapping[Byte,Char](_.toChar) compose worder compose printlnToConsole
    iterate(in)(it2)
    
    println("--------------")
    
    val it3 = charsetDecoder("UTF-8") compose worder compose ActorIteratee(Test.ConsoleActors.SysoutActor)
    iterate(in)(it3)

    Noop
  }

}