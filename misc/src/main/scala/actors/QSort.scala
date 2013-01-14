package actors

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.ActorRef

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
  case class Finished(val actorName: String, val elementCount: Int)

  case class Start[T](val pivot: T)

  // more messages classes here if needed

  def apply[T <% Ordered[T]](array: Array[T]) = array.sorted

  /**
   * It's not in-place sort (yet)!
   */
  private def actorSort[T <% Ordered[T]](array: Array[T])(
    parallelFactor: Int = Runtime.getRuntime().availableProcessors() * 2) = {

    val actorsNumber = parallelFactor * 2

    // make it not that simple
    val pivot = array(array.length / 2)

    val system = ActorSystem("QSort")

    // 1) Create coordinator actor
    val coordinatorActor = system.actorOf(Props(new Actor {

      private var finishedActors = Set[String]()

      def receive = {
        case Finished(actorName, elementCount) => {
          finishedActors += actorName
          if (finishedActors.size == parallelFactor * 2) {
            // all actors stopped their work, so we must regroup actors
            // TODO 

          }
        }
        case _ =>
      }
    }))

    // 2) create "left" and "right" actors and give them array chunks
    val leftScanners = (0 to parallelFactor) map (i =>
      system.actorOf(Props(
        new ChunkScanner(array, i, pivot) {
          val chunkSize = array.length / actorsNumber
          val isLeft = true
          val coordinator = coordinatorActor
        }), name = "LeftScanner_" + i))

    val rightScanners = (0 to parallelFactor) map (i =>
      system.actorOf(Props(
        new ChunkScanner(array, i, pivot) {
          val chunkSize = array.length / actorsNumber
          val isLeft = false
          val coordinator = coordinatorActor
        }), name = "RightScanner_" + i))

    // 3) 

  }

  /**
   *
   */
  abstract class ChunkScanner[T <% Ordered[T]](
    val array: Array[T],
    val number: Int,
    var pivot: T) extends Actor {

    // TODO Make it something real
    private val maxSize = 1

    protected val chunkSize: Int

    protected val isLeft: Boolean
    protected val coordinator: ActorRef

    private var receivedElements = Vector[T]()
    private var spareSize = 0
    
    private var currentOffset = number * chunkSize
    private var elementCount  = chunkSize
    
    def receive = {
      case x: Elements[T] => {
        val e = x.elements
        val esize = e.size
        if (spareSize < esize)
          spareMoreElements

        // could not find enough space to spare
        insertElements(e)
      }
      case _ =>
    }


    def spareMoreElements {
      val stop = false
      val nextOffset = (number + 1) * chunkSize
      var toSend = List[T]()
      var listSize = 0
      while (currentOffset < nextOffset) {
        val a = array(currentOffset)
        if (mustBeSwaped(pivot, a)) {
          // add to the queue to send
          toSend = a :: toSend
          listSize += 1
          // TODO Add marking empty spaces for the sent elements
          if (listSize >= maxSize) {
            receiver ! Elements(toSend)
            toSend = List[T]()
            listSize = 0
          }
        }
        currentOffset += 1
      }
      if (!toSend.isEmpty)
        receiver ! Elements(toSend)

      if (currentOffset == nextOffset) {
        // we're at the end of the array
        coordinator ! Finished(self.path.name, elementCount)
      }
    }

    private def mustBeSwaped(p: T, elem: T) = if (isLeft) p < elem else p > elem
    
    
    def receiver: ActorRef = {
//      otherSideActors
    }

    def insertElements(e: List[T]) {

    }

  }

}




