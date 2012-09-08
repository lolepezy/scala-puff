package ru.hflabs.testtask

import scala.util.Random
import scala.collection.immutable.TreeSet
import scala.collection.mutable.ListBuffer

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

sealed abstract case class SuffixNode[Code <% Ordered[Code]](code: Code) {
}

case class SuffixLeafNode[Code <% Ordered[Code], P](
  val c: Code, val payload: P)
  extends SuffixNode[Code](c) {

}

case class SuffixXNode[Code <% Ordered[Code]](
  val c: Code,
  var children: List[SuffixNode[Code]]) extends SuffixNode[Code](c) {

  def +=(child: SuffixNode[Code]) {
    children = child :: children
  }
}

object TreeFinder {

  def getSubsetPayloads[Code <% Ordered[Code], P](node: SuffixNode[Code], c: List[Code]): List[P] = {

    def gatherPayload(node: SuffixXNode[Code]): List[P] = {
      node.children.map(c => c match {
        case leaf: SuffixLeafNode[Code, P] => List(leaf.payload)
        case sx: SuffixXNode[Code] => gatherPayload(sx)
      }).flatten
    }

    node match {
      case leaf: SuffixLeafNode[Code, P] => {
        c match {
          case x :: Nil if (x == leaf.code) => List(leaf.payload)
          case _ => List()
        }
      }
      case xnode @ SuffixXNode(code, children) => {
        c match {
          case x :: Nil if (x == code) => gatherPayload(xnode)
          case x :: rest => {
            if (x < code) List()
            else if (x == code)
              children.flatMap(getSubsetPayloads(_, rest)).toList
            else
              children.filter(x >= _.code).flatMap(getSubsetPayloads(_, c)).toList
          }
          case Nil => List()
        }
      }
    }
  }

  def add[Code <% Ordered[Code], P](
    node: SuffixXNode[Code],
    c: List[Code],
    payload: P): SuffixNode[Code] = {

    c match {
      case Nil => ()
      case x :: Nil => {
        // do nothing, we're in the already existing 
        // branch, it's totally ok.
      }
      case x :: rest if (x == node.code) => {
        val next = rest.head
        val existingChildren = node.children.filter(_.code == next)
        if (existingChildren.isEmpty) {
          val child = rest match {
            case y :: Nil => SuffixLeafNode[Code, P](y, payload)
            case _ => add(SuffixXNode[Code](next, List()), rest, payload)
          }
          node += child
        } else {
          rest match {
            case y :: Nil => {
              if (!existingChildren.exists(_ match {
                case SuffixLeafNode(_, _) => true
                case _ => false
              })) {
                node += SuffixLeafNode[Code, P](next, payload)
              }
            }
            case _ => {
              val subtree = existingChildren.filter(_ match {
                case SuffixXNode(_, _) => true
                case _ => false
              })
              if (subtree.isEmpty)
                node += add(SuffixXNode[Code](next, List()), rest, payload)
              else
                add(subtree.head match { case s @ SuffixXNode(_, _) => s }, rest, payload)
            }
          }
        }
      }
      case _ => throw new Exception("This should not happen")
    }
    node
  }

}

/**
 * The class containing mapping of codes to suffix trees.
 * The key corresponds to the first symbol of a string.
 */
class SuffixTree[Code <% Ordered[Code], P](
  val trees: Array[Tuple2[Code, SuffixXNode[Code]]]) {

  type NodeType = SuffixXNode[Code]

  def findTreeIndex(c: Code): Option[Int] = {

    def binarySearch(c: Code, begin: Int, end: Int): Int = {
      val d = end - begin
      if (d <= 1)
        begin
      else {
        val m = d / 2
        val x = trees(m)._1
        if (x == c) m
        else if (x < c)
          binarySearch(c, m, end)
        else
          binarySearch(c, begin, m)
      }
    }

    if (trees.isEmpty) None
    else if (c < trees(0)._1) None
    else if (c < trees.last._1)
      Some(trees.length - 1)
    else
      Some(binarySearch(c, 0, trees.length))
  }

  /**
   * Process all tree with index smaller that "index".
   */
  private def traverseTrees[R](index: Int, f: NodeType => R): List[R] = {
    var i = 0;
    var b = new ListBuffer[R]
    println("index - i = " + (index - i))
    while (i <= index) {
      b += f(trees(i)._2)
      i = i + 1
    }
    b.toList
  }

  def find(c: List[Code]): List[P] = c match {
    case Nil => List()
    case x :: rest => {
      findTreeIndex(x) match {
        case None => List()
        case Some(index) => {
          traverseTrees(index, TreeFinder.getSubsetPayloads(_, c)).flatten
        }
      }
    }
  }

  override def toString() = "{ " + trees.map(x => x._1 + "->" + x._2).mkString(",") + " }"
}

object SuffixTreeHelper {

  type DefaultNodeType = SuffixXNode[Defs.CodeType]
  type DefaultLeafType = SuffixLeafNode[Defs.CodeType, Int]
  type DefaultTreeType = SuffixTree[Defs.CodeType, Int]

  def create(lines: Defs.EncLinesType) = {

    def makeSuffixTree(c: List[Defs.CodeType], payload: Int): SuffixNode[Defs.CodeType] = {
      c match {
        case x :: Nil => SuffixLeafNode(x, payload)
        case x :: rest =>
          SuffixXNode(x, List[SuffixNode[Defs.CodeType]](makeSuffixTree(rest, payload)))
      }
    }

    val maps = Util.timed("creation") {
      val m = new scala.collection.mutable.HashMap[Defs.CodeType, SuffixXNode[Defs.CodeType]]
      var index = 0
      for (line <- lines if (!line.isEmpty)) {
        val firstCode = line.head
        val tree = m.get(firstCode)
        tree match {
          case None => {
            val xnode = SuffixXNode(firstCode, List(makeSuffixTree(line.tail, index)))
            m.put(firstCode, xnode)
          }
          case Some(z) => TreeFinder.add(z, line, index)
        }
        index = index + 1
      }
      m
    }
    val g: Array[Tuple2[Defs.CodeType, DefaultNodeType]] = maps.toArray
    new DefaultTreeType(scala.util.Sorting.stableSort(g, (x, y) => x._1 < y._1))
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

    var optimalSubsets = new scala.collection.immutable.HashSet[Set[Defs.CodeType]]
    optimalSubsets += line

    var minSize = line.size
    var smallerSubsetsFound = true

    var findCount = 0;
    Util.timed("setCycle") {
      while (smallerSubsetsFound) {
        val next = optimalSubsets.foreach(us => {
          // find smaller subsets
          val subsets = us.map(us - _)
          val betterSubsets = subsets.filter(s =>
            !{ findCount += 1; tree.find(s.toList).exists(_ != index) }).filter(!_.isEmpty)

          if (betterSubsets.isEmpty) {
            if (us.size > minSize)
              optimalSubsets -= us
            smallerSubsetsFound = false
          } else {
            smallerSubsetsFound = true
            optimalSubsets -= us
            optimalSubsets ++= betterSubsets.filter(_.size <= minSize)
            minSize = betterSubsets.foldLeft(Int.MaxValue)((mSize, ss) => {
              val size = ss.size
              if (size < mSize) size else mSize
            })
          }
        })
        optimalSubsets = optimalSubsets.filter(_.size <= minSize)
        println("optimalSubsets.size = " + optimalSubsets.size)
      }
      println("findCount = " + findCount)
    }


    minSize = optimalSubsets.foldLeft(Int.MaxValue)((minSize, ss) => {
      val size = ss.size
      if (size < minSize) size else minSize
    })

    optimalSubsets.filter(_.size == minSize).toSet
  }

  def searchUniqueSubset[Code, P](lines: Defs.LinesType) = {
    var (encodedLines, mapping) = SubstringHelper.encodeLines(lines)
    encodedLines = encodedLines.map(e => e.sortWith(_ <= _))
    val tree = create(encodedLines)

    var index = 0;
    var x = new scala.collection.immutable.HashMap[Int, Set[Set[Defs.CodeType]]]
    for (line <- encodedLines) yield {
//      println("index = " + index + ", line = " + line)
      val subsets = matchSubsets(new TreeSet[Defs.CodeType]() ++ line, index, tree)
      x += (index -> subsets)
      index += 1
    }
    val reverseMapping = mapping.map(kv => (kv._2, kv._1))
    x.map(ss => (ss._1, ss._2.map(t => t.map(reverseMapping(_)))))
  }

}

object SubstringHelper {

  /**
   * Generate random strings according to task conditions.
   */
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
}