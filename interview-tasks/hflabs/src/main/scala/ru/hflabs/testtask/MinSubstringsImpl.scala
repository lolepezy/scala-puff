package ru.hflabs.testtask

import scala.annotation.tailrec
import scala.util.Random
import scala.collection.immutable.TreeSet
import scala.collection.mutable.ListBuffer

object Defs {
  val minLineLength = 2;
  val maxLineLength = 5;
  val dictionarySize = 6;
  val stringPoolSize = 10;

  type LinesType = List[List[String]]
  type CodeType = Short
  type EncLineType = List[CodeType]
  type EncLinesType = List[List[CodeType]]
  type Index = Int
}

object SuffixTreeHelper {

  /**
   * Create subsets from the smallest to the largest ones.
   *
   */
  def getSubsets[Code](line: List[Code],
    index: Defs.Index,
    codeMap: CodeMap[Code, Defs.Index]): Set[List[Code]] = {

    type IndexSetType = scala.collection.mutable.HashSet[Defs.Index]
    type PairType = Tuple3[Code, Code, IndexSetType]

    val emptyMetaSet = Set[List[Code]]()

    /**
     * Extract all pair of codes in the given line
     */
    def getAllLinePairs(line: List[Code]): List[PairType] =
      line match {
        case Nil => List[PairType]()
        case x :: Nil => List[PairType]()
        case x :: rest => {
          rest.map(y => {
            codeMap(x, y) match {
              case None => throw new Exception("This should not happen, x = " + x + ", line = " + line)
              case Some(z) => (x, y, z)
            }
          }) ++ getAllLinePairs(rest)
        }
      }

    def getFirstCodeMap(codePairs: List[PairType]) = {
      val emptyMap = Map[Code, List[Tuple2[Code, IndexSetType]]]()
      codePairs.foldLeft(emptyMap)((m, x) => {
        m.get(x._1) match {
          case None => m + (x._1 -> List((x._2, x._3)))
          case Some(z) => m + (x._1 -> ((x._2, x._3) :: z))
        }
      })
    }

    /**
     *
     */
    @tailrec
    def getExtendedSubsets(trialSubsets: List[Tuple2[Vector[Code], IndexSetType]],
        firstCodeMap: Map[Code, List[Tuple2[Code, IndexSetType]]], 
        maxSubsetSize: Int): Set[List[Code]] = {
      if (trialSubsets.isEmpty)
        emptyMetaSet
      else {

        //        println("trialSubsets = " + trialSubsets)

        val goodSubsets = trialSubsets.filter(ss =>
          !ss._2.exists(_ != index))

        val extSubsets = trialSubsets.map(s => {
          val (codeList, indexSet) = s
          firstCodeMap.get(codeList.last) match {
            case None => List()
            case Some(z) => {
              z.map(q => {
                val (secondCode, indexSetNext) = q
                (codeList :+ secondCode, indexSet intersect indexSetNext)
              })
            }
          }
        }).flatten

        if (goodSubsets.isEmpty) {
          // if we are already reached the size of the maximal subset
          if (!extSubsets.exists(_._1.size >= maxSubsetSize))
            getExtendedSubsets(extSubsets, firstCodeMap, maxSubsetSize)
          else
            emptyMetaSet
        } else
          goodSubsets.map(_._1.toList).toSet
      }
    }

    // first try to find one-code unique subsets
    var _1CodeSubsets = line.filter(codeMap(_) match {
      case None => false
      case Some(x) => !x.exists(_ != index)
    }).map(List(_)).toSet

    //    println("index = " + index)
    //    println("_1CodeSubsets = " + _1CodeSubsets)

    if (!_1CodeSubsets.isEmpty)
      _1CodeSubsets
    else {
      val codePairs = getAllLinePairs(line)
      //      println("codePairs = " + codePairs)
      val goodPairs = codePairs.filter(x => {
        !x._3.exists(_ != index)
      })
      //      println("goodPairs = " + goodPairs)
      if (!goodPairs.isEmpty)
        goodPairs.map(x => List(x._1, x._2)).toSet
      else {
        val firstCodeMap = getFirstCodeMap(codePairs)
        var trialSubsets = codePairs.map(x => (Vector(x._1, x._2), x._3))
        getExtendedSubsets(trialSubsets, firstCodeMap, line.size)
      }
    }
  }

  def searchUniqueSubsets[Code, P](lines: Defs.LinesType) = {
    var (encodedLines, mapping) = SubstringHelper.encodeLines(lines)
    encodedLines = encodedLines.map(e => e.sortWith(_ <= _))

    var x = new scala.collection.immutable.HashMap[Int, Set[List[Defs.CodeType]]]
    var codeMap = new CodeMap[Defs.CodeType, Int]

    var index = 0;
    Util.timed("create codeMap") {
      encodedLines.foreach(line => { codeMap ++ (line, index); index += 1 })
    }

    val reverseMapping = mapping.map(kv => (kv._2, kv._1))

    //    println("encodedLines = " + encodedLines)
    //    println("codeMap = " + codeMap)

    index = 0
    Util.timed("getSubsets") {
      encodedLines.foreach(line => {
        val subsets = getSubsets(line, index, codeMap)
        //        println("subsets = " + subsets)
        x += (index -> subsets)
        index += 1
      })
    }

    //    println("x = " + x)
    x.map(ss => (ss._1.toInt, ss._2.map(t => t.map(reverseMapping(_)).toList.sortWith(_ < _))))
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


