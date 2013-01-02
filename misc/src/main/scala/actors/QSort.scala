package actors

import akka.actor.Actor

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
class ActorQSort {

  def generateActors = {

  }

}

/**
 * The list of element to be sent from one chunk-actor to another one.
 */
case class Elements[T](val elements: List[T])

/**
 *
 */
class ChunkScanner[T <% Ordered[T]](val array: Array[T], val offset: Int) extends Actor {

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



