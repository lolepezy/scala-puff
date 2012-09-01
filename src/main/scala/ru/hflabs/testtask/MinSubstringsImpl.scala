package ru.hflabs.testtask

object Defs {
  val minLineLength = 4;
  val maxLineLength = 20;
  val dictionarySize = 1000;

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
  val children: List[SuffixTreeNode[Code, P]],
  val payload: P) {

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

  def add(c: List[Code]) {
    // TODO Impement
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
}

object SuffixTreeHelper {

  type DefaultNodeType = SuffixTreeNode[Defs.CodeType, Int]
  type DefaultTreeType = SuffixTree[Defs.CodeType, Int]

  def create(lines: Defs.LinesType): DefaultTreeType = {

    def makeSuffixTree(c: List[Defs.CodeType]): DefaultNodeType = {
      /**
       * TODO Add correct payload as an line index (if needed).
       */
      c match {
        case x :: Nil => new DefaultNodeType(x, List[DefaultNodeType](), 0)
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
        case None => maps.put(firstCode, makeSuffixTree(line.tail))
        case Some(z) => z.add(line.tail)
      }
    }

    new DefaultTreeType(maps)
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

class SubstringHelper {
  def generateString() = {

  }
}