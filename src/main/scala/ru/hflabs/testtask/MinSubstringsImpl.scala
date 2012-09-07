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

/**
 *
 */
case class SuffixTreeNode[Code <% Ordered[Code], P](
  val code: Code,
  var children: List[SuffixTreeNode[Code, P]],
  var payload: Option[P]) {

  def this(code: Code, payload: Option[P]) = this(code, List[SuffixTreeNode[Code, P]](), payload)

  def find(c: List[Code]): List[P] = {
    c match {
      case x :: Nil if (x == code) => payload match {
        case None => List()
        case Some(p) => List(p)
      }
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
  def add(c: List[Code], _payload: P) {
    c match {
      case Nil => ()
      case x :: Nil => {
        // do nothing, we're in the already existing branch, it's totally ok. 
        payload = Some(_payload)
      }
      case x :: rest if (x == code) => {
        val next = rest.head
        val existingChild = children.filter(_.code == next)
        if (existingChild.isEmpty)
          children = new SuffixTreeNode[Code, P](next, Some(_payload)) :: children
        else
          existingChild.foreach(_.add(rest, _payload))
      }
      case _ => throw new Exception("This should not happen")
    }
  }
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

  def find[Code <% Ordered[Code], P](node: SuffixNode[Code], c: List[Code]): List[P] = {
    node match {
      case leaf: SuffixLeafNode[Code, P] => {
        c match {
          case x :: Nil if (x == leaf.code) => List(leaf.payload)
          case _ => List()
        }
      }
      case SuffixXNode(code, children) => {
        c match {
          case x :: rest => {
            if (x < code) List()
            else if (x == code)
              children.flatMap(find(_, rest)).toList
            else
              children.filter(x >= _.code).flatMap(find(_, c)).toList
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
                add(subtree.head match { case s @ SuffixXNode(_, _) => s } , rest, payload)
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
  val trees: Array[Tuple2[Code, SuffixTreeNode[Code, P]]]) {

  type NodeType = SuffixTreeNode[Code, P]

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
    while (i <= index) {
      b += f(trees(i)._2)
      i = i + 1
    }
    b.toList
  }

  /**
   * Match the input line over the tree.
   */
  def matches(c: List[Code]): Boolean = c match {
    case Nil => false
    case x :: Nil => {
      findTreeIndex(x) match {
        case None => false
        case Some(index) => true
      }
    }
    case x :: rest => {
      findTreeIndex(x) match {
        case None => false
        case Some(index) =>
          traverseTrees(index, _.matches(c)).foldLeft(false)(_ || _)
      }
    }
  }

  def find(c: List[Code]): List[P] = c match {
    case Nil => List()
    case x :: rest => {
      findTreeIndex(x) match {
        case None => List()
        case Some(index) => {
          traverseTrees(index, _.find(c)).flatten
        }
      }
    }
  }

  override def toString() = "{ " + trees.map(x => x._1 + "->" + x._2).mkString(",") + " }"
}

object SuffixTreeHelper {

  type DefaultNodeType = SuffixTreeNode[Defs.CodeType, Int]
  type DefaultTreeType = SuffixTree[Defs.CodeType, Int]

  def create(lines: Defs.EncLinesType) = {

    def makeSuffixTree(c: List[Defs.CodeType], payload: Int): DefaultNodeType = {
      c match {
        case x :: Nil => new DefaultNodeType(x, None)
        case x :: rest =>
          new DefaultNodeType(x, List[DefaultNodeType](makeSuffixTree(rest, payload)), Some(payload))
      }
    }
    val maps = Util.timed("creation") {
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
    val g: Array[Tuple2[Defs.CodeType, DefaultNodeType]] = maps.toArray
    new DefaultTreeType(scala.util.Sorting.stableSort(g, (x, y) => x._1 < y._1))
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

    println("line=" + line)
    println("line.map(line - _)=" + line.map(line - _))
    println("tree=" + tree)
    println("index=" + index)

    val emptyMetaSet = Set[Set[Defs.CodeType]]()
    var uniqueSubsets = emptyMetaSet ++
      line.map(line - _).filter(s =>
        {
          val x = tree.find(s.toList)
          println("s.toList =" + s.toList)
          println("x =" + x)
          x.exists(_ != index)
        })

    println("uniqueSubsets=" + uniqueSubsets)

    var smallerSubsetsFound = true
    while (smallerSubsetsFound) {
      val next = uniqueSubsets.map(us => {
        // find smaller subsets
        val subsets = us.map(us - _)
        val smallerSubsets = subsets.filter(s =>
          tree.find(s.toList).exists(_ != index))

        println("subsets=" + subsets)
        println("smallerSubsets=" + smallerSubsets)

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
    x
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