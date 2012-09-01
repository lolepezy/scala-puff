package ru.hflabs.testtask

import scala.util.Random

object Defs {
  val minLineLength = 4;
  val maxLineLength = 20;
  val dictionarySize = 1000;
  val stringPoolSize = 10000;

  type CodeType = Short
  type LineType = List[String]
  type LinesType = List[List[String]]
  type EncLineType = List[List[CodeType]]
  type Index = Int
}

/**
 *
 */
case class SuffixTreeNode[Code <% Ordered[Code], P](
  val code: Code,
  var children: List[SuffixTreeNode[Code, P]],
  val payload: P) {

  def this(code: Code, payload: P) = this(code, List[SuffixTreeNode[Code, P]](), payload)

  def find(c: List[Code]): List[P] = {
    c match {
      case x :: Nil if (x == code) => List(payload)
      case x :: rest => {
        if (x < code) List()
        else if (x == code)
          children.flatMap(_ find rest).toList
        else
          children.filter(x >= _.code).flatMap(_ find c).toList
      }
      case Nil => List()
    }
  }

  def matches(c: List[Code]): Boolean = {
    def passToChildren(x: Code) =
      children.filter(x >= _.code).map(_ matches c).foldLeft(false)(_ || _)
    c match {
      case Nil => false
      case x :: Nil => {
        if (x == code) true
        else passToChildren(x)
      }
      case x :: rest => {
        if (x < code) false
        else if (x == code)
          children.map(_ matches rest).foldLeft(false)(_ || _)
        else
          passToChildren(x)
      }
    }
  }

  /**
   * Create new tree path for the given code string
   *
   * 1) A->(B,C) + AD = A->(B,C,D)
   * 2) A->(B,C) + AB = _
   * 3) A->(B,C) + ACD = A->(B,C->(D))
   *
   */
  def add(c: List[Code], payload: P) {
    c match {
      case Nil => ()
      case x :: Nil => {
        // do nothing, we're in the already existing branch, it's totally ok. 
      }
      case x :: rest if (x == code) => {
        val next = rest.head
        val existingChild = children.filter(_.code == next)
        existingChild.length match {
          case 0 => children = new SuffixTreeNode[Code, P](next, payload) :: children
          case 1 => existingChild.foreach(_.add(rest, payload))
          case _ => throw new Exception("This should not happen: c = " + c + ", x = " + x + ", code = " + code)
        }
      }
      case _ => throw new Exception("This should not happen")
    }
  }
}

/**
 * The class containing mapping of codes to suffix trees.
 * The key corresponds to the first symbol of a string.
 */
class SuffixTree[Code <% Ordered[Code], P](
  val initialNodes: scala.collection.Map[Code, SuffixTreeNode[Code, P]]) {

  /**
   * Match the input line over the tree.
   */
  def matches(c: List[Code]): Boolean = c match {
    case Nil => false
    case x :: Nil => initialNodes contains x
    case x :: rest => {
      val tree = initialNodes.get(x)
      tree match {
        case None => false
        case Some(z) => z.matches(rest)
      }
    }
  }

  override def toString() = "{ " + initialNodes + " }"
}

object SuffixTreeHelper {

  type DefaultNodeType = SuffixTreeNode[Defs.CodeType, Int]
  type DefaultTreeType = SuffixTree[Defs.CodeType, Int]

  def create(lines: Defs.LinesType) = {

    def makeSuffixTree(c: List[Defs.CodeType]): DefaultNodeType = {
      /**
       * TODO Add correct payload such as line index (if needed).
       */
      c match {
        case x :: Nil => new DefaultNodeType(x, 0)
        case x :: rest =>
          new DefaultNodeType(x, List[DefaultNodeType](makeSuffixTree(rest)), 0)
      }
    }

    val (encodedLines, mapping) = encodeLines(lines)
    val maps = new scala.collection.mutable.HashMap[Defs.CodeType, DefaultNodeType]
    for (line <- encodedLines if (line.size > 0)) {
      val firstCode = line(0)
      val tree = maps.get(firstCode)
      tree match {
        case None => maps.put(firstCode, makeSuffixTree(line))
        case Some(z) => z.add(line, 0)
      }
    }

    (new DefaultTreeType(maps), mapping)
  }

  /**
   * Encode words in lines with some integer (actually "LocalDefs.CodeType") values
   * for better performance of further comparisons.
   */
  def encodeLines(lines: Defs.LinesType) = {
    val mapping = new scala.collection.mutable.HashMap[String, Defs.CodeType]
    var currentCode = 0;
    val encLineList = lines.map(line => {
      for (word <- line) yield {
        val s = mapping.get(word)
        s match {
          case None => {
            val code = currentCode.asInstanceOf[Defs.CodeType]
            mapping.put(word, code)
            currentCode += 1
            code
          }
          case Some(x) => x
        }
      }
    })

    (encLineList, mapping)
  }
}

object SubstringHelper {
  def generateStrings(): Defs.LinesType = {
    val r = new Random()
    val dictionary = (
      for (i <- 0 until Defs.dictionarySize)
        yield "str" + i).toArray

    (for (i <- 0 until Defs.stringPoolSize) yield {
      val length = Defs.minLineLength + r.nextInt().abs % (Defs.maxLineLength - Defs.minLineLength)
      (for (j <- 0 until length) yield {
        val symbolIndex = (r.nextInt() % Defs.dictionarySize).abs
        dictionary(symbolIndex)
      }).toList
    }).toList
  }
}