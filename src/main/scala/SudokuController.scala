package SudokuController
import SudokuView._
import SudokuModel._

import scala.swing._
import scala.swing.event._

class SudokuController(val sudokuModel: SudokuModel) extends SimpleSwingApplication {

  // Load the main view and attach to the application
  override val top = SudokuView.main

  // Get events published by the view
  listenTo(SudokuView)

  // Link events from the view to appropriate handlers in the model
  reactions += {
    case SudokuView.TimeToSolve(grid) => {
      sudokuModel.setGrid(grid)
      SudokuView.displaySolution(sudokuModel.solve())
    }
  }
}