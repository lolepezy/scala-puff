package actors

import akka.transactor.Coordinated
import scala.concurrent.stm.Ref
import akka.actor.{ Actor, ActorRef }
import akka.actor.actorRef2Scala

case class Increment(friend: Option[ActorRef] = None)
case object GetCount

class Counter extends Actor {
  val count = Ref(0)
  def receive = {
    case coordinated @ Coordinated(Increment(friend)) => {
      friend foreach (_ ! coordinated(Increment()))
      coordinated atomic { implicit t =>
        count transform (_ + 1)
      }
    }
    case GetCount => sender ! count.single.get
  }
}
//
//val counter1 = Actor.actorOf[Counter].start()
//val counter2 = Actor.actorOf[Counter].start()
//counter1 ! Coordinated(Increment(Some(counter2)))
//...
//counter1 !! GetCount // Some(1)
