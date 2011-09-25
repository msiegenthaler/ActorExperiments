import actor._
import execution._
import annotation._

object Test extends impls.MainActor {

  object CounterActor {
    def spawn(recv: Actor[Int]) = {
      spawnPure(new Counter(recv).body(0))(Pooled)
    }

    private class Counter(recv: Actor[Int]) {
      def body(soFar: Int)(msg: Any): Action[Any] = {
        val nc = soFar + 1
        Send(recv, nc) continue body(nc)
      }
    }
  }

  object ConsoleActors extends ActorImplementor {
    val SysoutActor: Actor[String] = new SysoutActor
    private class SysoutActor extends ActorImpl[String] {
      override def processMessage(m: String) = {
        println("> " + m)
        Noop
      }
    }

    val SysinActor: Actor[Actor[String]] = new SysinActor
    private class SysinActor extends ActorImpl[Actor[String]] {
      private var subscribers: List[Actor[String]] = Nil

      start
      private def start = Thread {
        while (true) {
          val l = readLine
          synchronized {
            subscribers.foreach(to ⇒ handleAction(Send(to, l)))
          }
        }
      }

      override def processMessage(m: Actor[String]) = synchronized {
        subscribers = m :: subscribers
      }
    }
  }

  object Duplicator {
    def apply[T](as: Actor[T]*)(implicit s: ExecutionStrategy = Sequential): Actor[T] = spawnStateless[T] { msg ⇒
      as.map(Send(_, msg))
    }(s)
  }

  object Counter2 {
    def apply(): (Actor[Any], Actor[Actor[Int]]) = {
      val a = spawnPure(body(0))(Pooled)
      (a.comap(x ⇒ Token), a.comap(RequestCount(_)))
    }
    private sealed trait Request
    private object Token extends Request
    private case class RequestCount(requestor: Actor[Int]) extends Request

    private def body(soFar: Int)(msg: Request): Action[Request] = msg match {
      case Token           ⇒ body(soFar + 1) _
      case RequestCount(r) ⇒ r ! soFar
    }
  }

  object Periodic extends ActorImplementor {
    import java.util._
    private val timer = new Timer

    case class Subscription(actor: Actor[Unit], intervalMs: Int)

    val periodic = new ActorImpl[Subscription] {
      override def processMessage(sub: Subscription) = {
        val task = new TimerTask {
          override def run = {
            handleAction(sub.actor ! ())
            ()
          }
        }
        timer.schedule(task, sub.intervalMs, sub.intervalMs)
      }
    }
  }

  import ConsoleActors._
  import Periodic._
  import Pooled._

  def body(args: Array[String]) = {
    val outCount: Actor[Int] = SysoutActor.comap("Character-Count so far is " + _.toString)
    val outChar: Actor[Char] = SysoutActor.comap("Input was " + _)

    val (counter, reqCount) = Counter2()

    val duplicater = Duplicator(counter, outChar)(Pooled)

    val stringSplitter = spawnStateless[String] { msg ⇒
      msg.map(Send(duplicater, _))
    }

    val in = SysinActor

    val fetchCount = spawnStateless[Unit] { msg ⇒
      reqCount ! outCount
    }

    (in ! stringSplitter) + (periodic ! Periodic.Subscription(fetchCount, 1000))
  }
}
