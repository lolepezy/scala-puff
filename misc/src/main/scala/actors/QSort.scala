package actors

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props

/**
 * Actor-based qsort algorithm.
 *
 * How it should work:
 *
 * 1) Split array into two halfs with central pivot element
 * 2) Split array halfs into chunks and set every chunk to an actor.
 * 3) Every actor send elements below the pivot to the left and sends
 * elements above the pivot to the right.
 * 4) After all actors have empty element queue to send.
 *
 * Why it sucks: actor cannot send elements and and receive them at the
 * same time, so possibly we need a pair of actors here. We also possibly
 * need STM here.
 *
 *
 */
object QSort {

  /**
   * Messages case classes.
   *
   * The list of element to be sent from one chunk-actor to another one.
   */
  case class Elements[T](val elements: List[T])

  /**
   * To be sent by an actor who stopped sending elements.
   */
  case class Finished(val actorName: String)

  case class Start[T](val pivot: T)

  // more messages classes here if needed

  def apply[T <% Ordered[T]](array: Array[T]) = array.sorted

  /**
   * It's not in-place sort (yet)!
   */
  private def actorSort[T <% Ordered[T]](array: Array[T])(
    parallelFactor: Int = Runtime.getRuntime().availableProcessors() * 2) = {

    // 1) create "left" and "right" actors and give them array chunks
    val system = ActorSystem("QSort")
    val leftScanners = (0 to parallelFactor) map (i =>
      system.actorOf(Props(new ChunkScanner(array, i)), name = "LeftScanner_" + i))
    val rightScanners = (0 to parallelFactor) map (i =>
      system.actorOf(Props(new ChunkScanner(array, i)), name = "RightScanner_" + i))

    // 2) Create coordinator actor
    val coordinator = new Actor {

      private var finishedActors = Set[String]()

      def receive = {
        case Finished(actorName) => {
          finishedActors += actorName
          if (finishedActors.size == parallelFactor * 2) {
            // all actors stopped their work, so we must regroup actors
            // TODO 
            
          }
        }
        case _ =>
      }
    }

    // 3) 
  }

  /**
   *
   */
  class ChunkScanner[T <% Ordered[T]](val array: Array[T], val number: Int) extends Actor {

    private var spareSize = 0

    def receive = {
      case x: Elements[T] => {
        val e = x.elements
        val esize = e.size
        if (spareSize < esize) {
          spareMoreElements
        }
        // could not find enough space to spare
        if (spareSize < esize) {
          if (spareSize > 0)
            resendToNextActor(e)
          else {
            // place "spareSize" of them here and send other to other actors

          }
        }

        insertElements(e)
      }
      case _ =>
    }

    def spareMoreElements {

    }

    def resendToNextActor(e: List[T]) {

    }

    def insertElements(e: List[T]) {

    }

  }

}




