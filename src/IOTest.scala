import actor._
import impls._
import io.iteratee._
import Iteratee._
import IterateeFun._
import execution._
import annotation._
import IterateeActor._

object IOTest extends MainActor {

  object Console extends ActorImplementor {
    object Reader {
      private type Subscribers = List[ActionIteratee[Byte]]
      private val newSubscribers = new java.util.concurrent.atomic.AtomicReference[Subscribers](Nil)

      val actor: Actor[ActionIteratee[Byte]] = {
        new ActorImpl[ActionIteratee[Byte]] {
          @tailrec override def processMessage(it: ActionIteratee[Byte]) = {
            val subs = newSubscribers.get
            if (!newSubscribers.compareAndSet(subs, it :: subs)) processMessage(it)
          }
        }
      }

      @tailrec private def fetchNewSubscribers: Subscribers = {
        val subs = newSubscribers.get
        if (!subs.isEmpty && !newSubscribers.compareAndSet(subs, Nil)) fetchNewSubscribers
        else subs
      }

      start
      private def start = {
        val reader = System.in

        @tailrec def handle(data: Input[Byte])(left: Subscribers, handled: Subscribers = Nil): Subscribers = left match {
          case it :: l ⇒
            val nit = it(data)
            nit.outOption.foreach(handleAction)
            handle(data)(l, nit :: handled)
          case Nil ⇒ handled //let's not reverse, since we don't guarantee order anyway
        }
        @tailrec def run(subs: Subscribers) {
          val in = reader.read
          val data = if (in == -1) EOF else Data(in.toByte)
          val nsubs = handle(data)(fetchNewSubscribers ::: subs)
          run(nsubs)
        }
        Thread(run(Nil))
      }
    }

    object Writer {
      val actor: Actor[String] = new ActorImpl[String] {
        override def processMessage(m: String) = {
          queue offer m
          Noop
        }
      }

      private val queue = new java.util.concurrent.LinkedBlockingQueue[String]
      start
      private def start = Thread {
        while (true) {
          val line = queue.take
          println(line)
        }
      }
    }
  }

  def worder = {
    def string(chars: List[Char]) = new String(chars.reverse.toArray)
    def wf(buffer: List[Char])(in: Input[Char]): Iteratee[Char, String] = in match {
      case Data(char) if char.isWhitespace ⇒
        if (buffer.isEmpty) cont(wf(Nil))
        else cont(wf(Nil), string(buffer))
      case Data(char) ⇒ cont(wf(char :: buffer))
      case Empty      ⇒ cont(wf(buffer))
      case EOF        ⇒ done(string(buffer))
    }

    cont(wf(Nil))
  }

  def charsetDecoder(charset: String) = {
    def decode(charset: String)(buffer: Array[Byte], len: Int)(in: Input[Byte]): Iteratee[Byte, Char] = in match {
      case Data(byte) ⇒
        val nl = len + 1
        val b = if (nl >= buffer.length) new Array[Byte](buffer.length * 2) else buffer
        buffer(len) = byte
        val s = new String(buffer, 0, nl, charset)
        if (s.length > 0) {
          val char = s.charAt(0)
          val f = decode(charset)(buffer, 0) _
          cont(f, char)
        } else cont(decode(charset)(buffer, nl))
      case Empty ⇒ cont(decode(charset)(buffer, len))
      case EOF   ⇒ done
    }

    cont(decode(charset)(new Array(10), 0))
  }

  def printlnToConsole = {
    def print(in: Input[String]): Iteratee[String, Unit] = {
      in match {
        case Data(d) ⇒
          println(d)
          cont(print)
        case Empty ⇒ cont(print)
        case EOF   ⇒ done
      }
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
        case Data(d) ⇒ cont(handle, d)
        case Empty   ⇒ cont(handle)
        case EOF     ⇒ done
      }
    }
    cont(handle)
  }

  def iterate[E, O](l: List[E])(it: Iteratee[E, O]): Unit = {
    val (in, t) = l match {
      case e :: t ⇒ (Data(e), t)
      case Nil    ⇒ (EOF, Nil)
    }
    val nit = it(in)
    if (nit.isDone) ()
    else iterate(t)(nit)
  }

  override def body(args: Array[String]) = {
    val text = "Mario is doing some tests tonight"
    val in = text.getBytes("UTF-8").toList

    val it1 = charsetDecoder("UTF-8") |> worder |> mapping(_.toUpperCase) |> printlnToConsole
    iterate(in)(it1)

    println("--------------")

    val it2 = mapping[Byte, Char](_.toChar) |> worder |> printlnToConsole
    iterate(in)(it2)

    println("--------------")

    println("Enter input: ")
    val consoleIt = charsetDecoder("UTF-8") |> worder |> sendTo(Console.Writer.actor)
    Console.Reader.actor ! consoleIt
  }

}