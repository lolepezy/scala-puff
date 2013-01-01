package actors

import akka.actor.{ Actor, ActorRef, Props }
import akka.pattern._
import org.junit._
import scala.util.Random
import akka.actor.PoisonPill
import scala.collection.mutable.BitSet
import akka.util.duration._
import akka.actor.actorRef2Scala
import org.junit.Test

case class Generate(begin: Int, count: Int)
case class GeneratedChunk(begin: Int, chunk: List[Int])
case class Sum(begin: Int, chunk: List[Int])
case class SumResult(begin: Int, sum: Long)

class SummingActor() extends Actor {
  def receive = {
    case Sum(begin, chunk) => {
      val sum = chunk.foldLeft(0)(_ + _)
      sender ! SumResult(begin, sum)
    }
    case m @ _ => println("Wrong! " + m)
  }
}

class GeneratorActor() extends Actor {
  def receive = {
    case Generate(begin, count) => {
      val r = new Random
      val chunk = (0 until count).map(x => r.nextInt).toList
      sender ! GeneratedChunk(begin, chunk)
    }
    case m @ _ => println("Wow! " + m)
  }
}

class DispatchActor extends Actor {
  val N = 100000
  val parallelFactor = 10;
  val chunkSize = N / parallelFactor
  val values = new Array[Int](N)
  val generated = new BitSet(parallelFactor)

  var totalSum = 0L;
  var responseCount = 0;

  var generators = List[ActorRef]();
  var summingActors = List[ActorRef]();

  override def preStart() = {
    generators = (0 until parallelFactor).map(i => {
      val generator = context.actorOf(Props[GeneratorActor], name = "generator" + i)
      generator ! Generate(chunkSize * i, chunkSize)
      generator
    }).toList
  }

  def receive = {
    case GeneratedChunk(begin, chunk) => {
      val summingActor = context.actorOf(Props[SummingActor], name = "summingActor" + begin / chunkSize)
      summingActor ! Sum(begin, chunk)
      summingActors = summingActor :: summingActors
    }
    case SumResult(begin, sum) => {
      println("begin = " + begin + ",  sum = " + sum)
      totalSum += sum
      responseCount += 1
      if (responseCount == parallelFactor) {
        println("totalSum = " + totalSum)
        die
      }
    }

    case m @ _ => println("Oops " + m)
  }

  def die = {
    generators.foreach(_ ! PoisonPill)
    summingActors.foreach(_ ! PoisonPill)
    generators.map(gracefulStop(_, 5 seconds)(context.system))
    summingActors.map(gracefulStop(_, 5 seconds)(context.system))
    self ! PoisonPill
  }
}

class RandomActors {
  @Test def testRandom {
//    val actorSystem = ActorSystem("RandomActors")
//    val dispatcher = actorSystem.actorOf(Props[DispatchActor], name = "dispatcher")
//    val s = gracefulStop(dispatcher, 10 seconds)(actorSystem)
//    Await.result(s, 11 seconds)
  }

}