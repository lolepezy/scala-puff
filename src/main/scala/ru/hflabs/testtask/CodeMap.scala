package ru.hflabs.testtask
import scala.annotation.tailrec

class CodeMap[Code, P] {
  type Key = Tuple2[Code, Code]
  type SetType = scala.collection.mutable.HashSet[P]

  val codePairMap = new scala.collection.mutable.HashMap[Key, SetType]
  val codeSingleMap = new scala.collection.mutable.HashMap[Code, SetType]

  private def addPair(code1: Code, code2: Code, payload: P) = {
    val key = (code1, code2)
    val s = codePairMap.get(key)
    s match {
      case None => {
        val q = new SetType
        q.add(payload)
        codePairMap.put(key, q)
      }
      case Some(z) => z.add(payload)
    }
    this
  }

  private def addSingle(code: Code, payload: P) = {
    val s = codeSingleMap.get(code)
    s match {
      case None => {
        val q = new SetType
        q.add(payload)
        codeSingleMap.put(code, q)
      }
      case Some(z) => z.add(payload)
    }
    this
  }

  def ++(encLine: List[Code], payload: P) {
    encLine.foreach(addSingle(_, payload))
    addLinePairs(encLine, payload)
  }

  @tailrec
  private def addLinePairs(encLine: List[Code], payload: P) {
    encLine match {
      case Nil => ()
      case x :: Nil => ()
      case x :: rest => {
        rest.foreach(addPair(x, _, payload))
        addLinePairs(rest, payload)
      }
    }
  }

  def apply(code1: Code, code2: Code) = codePairMap.get((code1, code2))
  def apply(code: Code) = codeSingleMap.get(code)

  override def toString = "{ singles = " + codeSingleMap + ", pairs = " + codePairMap + "}"
}