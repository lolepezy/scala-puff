package tests.actors

import akka.actor.{ Actor, ActorRef, Props }
import org.junit._
import scala.util.Random
import scala.collection.mutable.BitSet

class RandomActors {

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
      case _ => println("Wow!")
    }
  }

  class GeneratorActor() extends Actor {
    def receive = {
      case Generate(begin, count) => {
        val r = new Random
        val chunk = (0 until count).map(x => r.nextInt).toList
        sender ! GeneratedChunk(begin, chunk)
      }
      case _ => println("Wow!")
    }
  }

  class DispatchActor extends Actor {
    val N = 1000000
    val parallelFactor = 10;
    val chunkSize = N / parallelFactor
    val values = new Array[Int](N)
    val generated = new BitSet(parallelFactor)

    var totalSum = 0L;

    override def preStart() = {
      (0 until parallelFactor).foreach(i => {
        val generator = context.actorOf(Props[GeneratorActor], name = "generator" + i)
        generator ! Generate(chunkSize * i, chunkSize)
      })
    }

    def receive = {
      case GeneratedChunk(begin, chunk) => {
        val generator = context.actorOf(Props[GeneratorActor], name = "summingActor" + begin / chunkSize)
        generator ! Sum(begin, chunk)
      }
      case SumResult(begin, sum) => {
        totalSum += sum
      }
      case _ => println("Wow 2!")
    }
  }

  @Test def testMean {

  }

}