package ru.hflabs.testtask

import scala.util.Random
import scala.collection.immutable.TreeSet

object Defs {
  val minLineLength = 4;
  val maxLineLength = 20;
  val dictionarySize = 1000;
  val stringPoolSize = 10000;

  type LinesType = List[List[String]]
  type CodeType = Short
  type EncLineType = List[CodeType]
  type EncLinesType = List[List[CodeType]]
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
        if (existingChild.isEmpty)
          children = new SuffixTreeNode[Code, P](next, payload) :: children
        else
          existingChild.foreach(_.add(rest, payload))
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

  def find(c: List[Code]): List[P] = c match {
    case Nil => List()
    case x :: rest => {
      val tree = initialNodes.get(x)
      tree match {
        case None => List()
        case Some(z) => z.find(rest)
      }
    }
  }

  override def toString() = "{ " + initialNodes + " }"
}

object SuffixTreeHelper {

  type DefaultNodeType = SuffixTreeNode[Defs.CodeType, Int]
  type DefaultTreeType = SuffixTree[Defs.CodeType, Int]

  def create(lines: Defs.EncLinesType) = {

    def makeSuffixTree(c: List[Defs.CodeType], payload: Int): DefaultNodeType = {
      c match {
        case x :: Nil => new DefaultNodeType(x, payload)
        case x :: rest =>
          new DefaultNodeType(x, List[DefaultNodeType](makeSuffixTree(rest, payload)), payload)
      }
    }
    val maps = Util.timed("creation") {
      () =>
        {
          val m = new scala.collection.mutable.HashMap[Defs.CodeType, DefaultNodeType]
          var index = 0
          for (line <- lines if (!line.isEmpty)) {
            val firstCode = line(0)
            val tree = m.get(firstCode)
            tree match {
              case None => m.put(firstCode, makeSuffixTree(line, index))
              case Some(z) => z.add(line, index)
            }
            index = index + 1
          }
          m
        }
    }
    new DefaultTreeType(maps)
  }

  /**
   * Encode words in lines with some integer (actually "LocalDefs.CodeType") values
   * for better performance of further comparisons.
   */
  def encodeLines(lines: Defs.LinesType) = {
    var mapping = new scala.collection.immutable.HashMap[String, Defs.CodeType]
    var currentCode = 0;
    val encLineList = lines.map(line => {
      for (word <- line) yield {
        val s = mapping.get(word)
        s match {
          case None => {
            val code = currentCode.asInstanceOf[Defs.CodeType]
            mapping += (word -> code)
            currentCode += 1
            code
          }
          case Some(x) => x
        }
      }
    })

    (encLineList, mapping)
  }

  // TreeSet is used to have sorted toList calls
  def matchSubsetsRecursive(line: TreeSet[Defs.CodeType],
    index: Defs.Index,
    tree: SuffixTreeHelper.DefaultTreeType): Set[Set[Defs.CodeType]] = {
    val subsets = line.map(x => line - x)
    subsets.map(x => {
      tree.find(x.toList).filter(_ != index) match {
        case Nil => {
          val smallerMatching = matchSubsetsRecursive(x, index, tree)
          if (smallerMatching.isEmpty)
            Set[Set[Defs.CodeType]]() + x
          else
            smallerMatching
        }
        case _ => Set[Set[Defs.CodeType]]()
      }
    }).foldLeft(Set[Set[Defs.CodeType]]())(_ ++ _).filter(!_.isEmpty)
  }

  // TreeSet is used to have sorted toList calls
  def matchSubsets(line: TreeSet[Defs.CodeType],
    index: Defs.Index,
    tree: SuffixTreeHelper.DefaultTreeType): Set[Set[Defs.CodeType]] = {

    val emptyMetaSet = Set[Set[Defs.CodeType]]()
    var uniqueSubsets = emptyMetaSet ++
      line.map(line - _).filter(s =>
        tree.find(s.toList).exists(_ != index))

    var smallerSubsetsFound = true
    while (smallerSubsetsFound) {
      val next = uniqueSubsets.map(us => {
        // find smaller subsets
        val subsets = us.map(us - _)
        val smallerSubsets = subsets.filter(s =>
          tree.find(s.toList).exists(_ != index))

        if (smallerSubsets.isEmpty)
          (emptyMetaSet + us, false)
        else
          (smallerSubsets, true)
      }).foldLeft((emptyMetaSet, false))((x, y) => (x._1 ++ y._1, x._2 || y._2))

      // if there were no "true" flags then there were 
      // no smaller subsets, so we can stop the loop
      smallerSubsetsFound = next._2
      if (smallerSubsetsFound)
        uniqueSubsets = next._1
    }
    uniqueSubsets
  }

  def searchUniqueSubset[Code, P](lines: Defs.LinesType) = {
    val (encodedLines, mapping) = encodeLines(lines)
    val tree = create(encodedLines)

    var index = 0;
    var x = new scala.collection.immutable.HashMap[Int, Set[Set[Defs.CodeType]]]
    for (line <- encodedLines) yield {
      val subsets = matchSubsets(new TreeSet[Defs.CodeType]() ++ line, index, tree)
      x += (index -> subsets)
      index = index + 1
    }
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