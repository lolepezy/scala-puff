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
  case class Finished(val actorId: String, val info: ScannerStatus)

  /**
   * Initializing message for every sort step.
   */
  case class Start[T](val pivot: T, val isLeft: Boolean, val otherSideActors: List[ActorRef])

  /**
   * The message to send itself as a signal that scanner needs to
   * continue scanning and sending elements.
   */
  case class Continue

  case class ScannerStatus(val elementCount: Int)

  // more messages classes here if needed

  /**
   * Scanners tree structure.
   */
  sealed case class ScannerTree

  case class ScannerSet[T](val scanners: List[T]) extends ScannerTree
  case class ScannerBranch(val left: ScannerTree, val right: ScannerTree) extends ScannerTree

  case class ScannerInfo(
    val id: String,
    val actor: ActorRef,
    val elementCount: Int)

  def apply[T <% Ordered[T]](array: Array[T]) = array.sorted

  /**
   * It's not in-place sort (yet)!
   */
  private def actorSort[T <% Ordered[T]](array: Array[T])(
    parallelFactor: Int = Runtime.getRuntime().availableProcessors() * 2) = {

    val actorHalfNumber = parallelFactor
    val actorsNumber = actorHalfNumber * 2

    // make it not that simple
    val pivot = array(array.length / 2)

    val system = ActorSystem("QSort")

    var scanners = ScannerTree()
    var scannerInfoMap = Map[String, ScannerInfo]()

    /**
     * Divide scanner left and right groups.
     *
     */
    def regroupScanners(scanTree: ScannerTree): ScannerTree = {
      scanTree match {
        case ScannerSet(s) => {
          val size = s.size
          val middle = size / 2

          val (left, right) = if (size % 2 == 0)
            s.zipWithIndex.partition(_._2 < middle)
          else {
            val zipped = s.zipWithIndex.map(x => x)
            val (l, r) = zipped.view.filter(_._2 != middle).partition(_._2 < middle)
            (l, r)
          }
          def list[T, Y](x: Seq[(T, Y)]) = x.map(_._1).toList
          ScannerBranch(ScannerSet(list(left)), ScannerSet(list(right)))
        }
        case ScannerBranch(left, right) => ScannerBranch(regroupScanners(left), regroupScanners(right))
      }
    }

    // 1) Create coordinator actor
    val coordinatorActor = system.actorOf(Props(new Actor {

      private var actorsInProcess = parallelFactor * 2
      private var finishedActors = Map[String, ScannerStatus]()

      private def allActorsDone = finishedActors.size == actorsInProcess

      def receive = {
        case Finished(actorId, ScannerStatus(elementCount)) => {
          scannerInfoMap.get(actorId).map(sim => {
//            ScannerInfo
          })
//          finishedActors += actorId -> scannerStatus
          if (allActorsDone) {
            // all actors stopped their work, so we must regroup actors
            // and start the new recursive step
            scanners = regroupScanners(scanners)
          }
        }
        case _ =>
      }
    }))

    // Auxiliary stuff
    def createScannerInfos(id: String, s: ActorRef) = {
      val si = ScannerInfo(id, s, 0)
      scannerInfoMap += id -> si
      (s, si)
    }

    // 2) create scanners "left" ++ "right" scanning actors and give them array chunks
    scanners = ScannerBranch(
      ScannerSet(
        (0 to actorHalfNumber).map(
          i => {
            val id = "scanner_" + i
            createScannerInfos(id, system.actorOf(Props(
              new ChunkScanner(array, i, pivot) {
                val chunkSize = array.length / actorsNumber
                val isLeft = true
                val coordinator = coordinatorActor
              }), name = id))
          }).toList),
      ScannerSet(
        (0 to actorHalfNumber).map(
          i => {
            val id = "scanner_" + (actorHalfNumber + i)
            createScannerInfos(id, system.actorOf(Props(
              new ChunkScanner(array, actorHalfNumber + i, pivot) {
                val chunkSize = array.length / actorsNumber
                val isLeft = false
                val coordinator = coordinatorActor
              }), name = id))
          }).toList))

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

    private var isLeft = false

    private var otherSideActors = List[ActorRef]()
    protected val coordinator: ActorRef

    private var receivedElements = Vector[T]()
    private var spareSize = 0

    private var currentOffset = number * chunkSize
    private var elementCount = chunkSize

    def receive = {

      case Continue => spareMoreElements

      case x: Elements[T] => {
        val e = x.elements
        val esize = e.size
        if (spareSize < esize)
          spareMoreElements

        insertElements(e)
        spareMoreElements
      }

      case x: Start[T] => {
        isLeft = x.isLeft
        pivot = x.pivot
        otherSideActors = x.otherSideActors
        spareMoreElements
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
          // TODO Add marking empty spaces for the elements sent 
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
        coordinator ! Finished(self.path.name, ScannerStatus(elementCount))
      } else {
        self ! Continue
      }
    }

    private def mustBeSwaped(p: T, elem: T) = if (isLeft) p < elem else p > elem

    def receiver: ActorRef = {
      null
    }

    def insertElements(e: List[T]) {

    }

  }

}




