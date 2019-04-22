package SudokuModel

class SudokuModel {

  // TODO: Identify unsolvable puzzles, inconsistent puzzles, or nonunique solutions
  // TODO: Optimize backtracking algorithm

  // Represent the sudoku grid as an array of arrays
  // TODO:    Represent the grid as a 1-dimensional array
  // TODO:    in order to optimize lookups and writes
  // TODO:    This will match the structure of the ViewModel
  private var grid: Array[Array[Int]] = new Array[Array[Int]](9)

  // converts ViewModel representation to jagged array representation
  // for the backtracking algorithm
  def setGrid(array: Array[Int]): Array[Array[Int]] = {
    grid = array.sliding(9, 9).toArray[Array[Int]]
    grid
  }

  def printGrid() = grid.foreach(row => {
    println(row.foldLeft[String]("")((s,x) => s + s"| ${x} |"))
  })

  //Only for printing solution to console

  def solutionString =
    grid
      .map(row => {
        row.foldLeft[String]("")((s, x) => s + s"| ${x} |")
      })
      .foldLeft[String]("\n")((s, x) => s + s"${x}\n")

  private var isSolved: Boolean = false
  private def getVal(row: Int, column: Int): Int = grid(row)(column)
  private def getVal(coords: (Int, Int)): Int = grid(coords._1)(coords._2)
  private def checkRow(row: Int, value: Int): Boolean = {
    grid(row).contains(value)
  }

  private def checkColumn(column: Int, value: Int): Boolean = {
    (0 to 8).foreach(row => if(grid(row)(column) == value) return true)
    return false
  }

  private def checkBlock(row: Int, column: Int, value: Int): Boolean = {
    (0 to 2).foreach(blockRow => {
      val rowOffset = (row / 3) * 3
      (0 to 2).foreach(blockColumn => {
        val columnOffset = (column / 3) * 3
        if(grid(blockRow + rowOffset)(blockColumn +  columnOffset) == value) return true
      })
    })
    false
  }

  private def isValidPlacement(coords: (Int, Int), value: Int): Boolean =
    !checkColumn(coords._2, value) &&
      !checkRow(coords._1, value) &&
      !checkBlock(coords._1, coords._2, value)

  // Mutually recursive with nextValue function
  // mutualBacktrack determines which entries to test values in
  // next value decides when to increment values to be tested, or returns
  // false when all possibles are exhausted
  //
  // TODO:    Make tail recursive if possible
  // TODO:    Or, replace with mutable stack and while loop

  def mutualBacktrack(index: Int, value: Int) : Boolean = {
    if(index == 81) return true
    val x: Int = index / 9
    val y: Int = index % 9

    if (grid(x)(y) != 0) return mutualBacktrack(index + 1, 1)
    if (isValidPlacement((x,y), value)) {
      grid(x)(y) = value
      if(mutualBacktrack(index + 1, 1)) return true
      else {
        grid(x)(y) = 0
        return nextValue(index, value)
      }
    }
    else nextValue(index, value)
  }

  def nextValue(index: Int, value: Int): Boolean = {
    if(value + 1 < 10) mutualBacktrack(index, value + 1) else false
  }

  def solve(): Array[Array[Int]] = {
    mutualBacktrack(0, 1)
    grid
  }
}