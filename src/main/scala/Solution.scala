import hw.sudoku._

object Solution extends SudokuLike {
  type T = Board

  def calcPeers(row: Int, col: Int): List[(Int, Int)] = {
    val rowPeers = 0.to(8).map{ r => (r, col) }
    val colPeers = 0.to(8).map{ c => (row, c) }
    val boxRow = (row / 3) * 3
    val boxCol = (col / 3) * 3
    val boxPeers = boxRow.to(boxRow + 2).flatMap {
      r => boxCol.to(boxCol + 2).map {c => (r,c) }
    }
    (rowPeers ++ colPeers ++ boxPeers).filterNot {
      case (r,c) => r == row && col == c
    }.toList.distinct
  }

  val peersTbl = Map((0.to(8).flatMap {
    r => 0.to(8).map {
      c => ((r, c) -> calcPeers(r, c))
    }
  }) :_*)

  def peers(row: Int , col: Int): List[(Int, Int)] = {
    calcPeers(row, col)
  }

  def parse(str: String): Board = {
    val empty = new Board(peersTbl.mapValues(x => List(1,2,3,4,5,6,7,8,9)))

    parseHelper(str, 0, 0, empty)
  }

  def parseHelper(str: String, row: Int, col: Int, added: Board): Board = {
    if (row > 8) {
      added
    } else if (col > 8) {
      parseHelper(str, row + 1, 0, added)
    } else {
      if (str.apply(0) == '.') {
        parseHelper(str.substring(1), row, col + 1, added)
      } else {
        parseHelper(str.substring(1), row, col + 1, added.place(row, col, str.charAt(0).asDigit))
      }
    }
  }

}

class Board(val available: Map[(Int, Int), List[Int]]) extends BoardLike[Board] {

  def availableValuesAt(row: Int, col: Int): List[Int] = {
    available.getOrElse((row, col), 1.to(9).toList)
  }

  def valueAt(row: Int, col: Int): Option[Int] = {
    val lst = available.get(row, col)

    val lstComplete = List(1,2,3,4,5,6,7,8,9)

    val res = lstComplete.filterNot(x => lst.contains(x))

    if (res == List()) {
      None
    } else {
      Some(res.head)
    }
  }

  def isSolved(): Boolean = {
    val res = available.mapValues(x => x.length)

    res.forall(x => x._2 == 1)
  }

  def isUnsolvable(): Boolean = {
    val res = available.mapValues(x => x.length)

    res.exists(x => x._2 == 0)
  }

  def place(row: Int, col: Int, value: Int): Board = {
    val inAvailable = available + ((row, col) -> (value :: Nil))

    new Board(removeFromPeers(Solution.calcPeers(row, col), inAvailable, value))
  }

  def removeFromPeers(peers: List[(Int, Int)], map: Map[(Int, Int), List[Int]], value: Int): Map[(Int, Int), List[Int]] = peers match {
    case Nil => map
    case h :: t => {
      val row = h._1
      val col = h._2
      val lst = map.get(row, col).get
      if (lst.contains(value)) {
        if (lst.size == 2) {
          val not = lst.filter(_ != value)
          removeFromPeers(peers.tail, removeFromPeers(Solution.calcPeers(row, col), map + (((row, col), not)), not.head) + (((row, col), lst.filter(_ != value))), value)
        } else {
          removeFromPeers(peers.tail, map + (((row, col), lst.filter(_ != value))), value)
        }
      } else {
        removeFromPeers(peers.tail, map, value)
      }
    }
  }

  def organize(): Int = {
    available.values.flatten.size
  }

  def nextStates(): Stream[Board] = {
    if (isUnsolvable ()) {
      Stream.empty
    } else {
      val lst = available.toList

      val states = nextStatesHelper(lst)

      states.sortWith(_.organize < _.organize)
    }
  }

  def nextStatesHelper(lst: List[((Int, Int), List[Int])]): Stream[Board] = lst match {
    case Nil => Stream.empty
    case h :: t => {
      val row = h._1._1
      val col = h._1._2
      val newList = h._2
      nextStatesHelperHelper(row, col, newList) #::: nextStatesHelper(t)
    }
  }

  def nextStatesHelperHelper(row: Int, col: Int, lst: List[Int]): Stream[Board] = lst match {
    case Nil => Stream.empty
    case h :: t => {
      place(row, col, h) #:: nextStatesHelperHelper(row, col, t)
    }
  }

  def solve(): Option[Board] = {
    val states = this.nextStates()

    if (this.isSolved()) {
      Some(this)
    } else {
      solveHelper(states)
    }
  }

  def solveHelper(states: Stream[Board]): Option[Board] = states match {
    case Stream.Empty => None
    case h #:: t => {
      if (h.isUnsolvable()) {
        None
      }
      if (h.isSolved()) {
        Some(h)
      } else {
        solveHelper(t #::: h.nextStates())
      }
    }
  }
}
