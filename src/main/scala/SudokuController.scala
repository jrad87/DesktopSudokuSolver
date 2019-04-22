import SudokuView._
import SudokuModel._

import scala.swing._
import scala.swing.event._

object SudokuController extends SimpleSwingApplication {

  // Load the main view and attach to the application
  override val top = SudokuView.main

  // Get events published by the view
  listenTo(SudokuView)


  val model = new SudokuModel()

  // Link events from the view to appropriate handlers in the model
  reactions += {
    case SudokuView.TimeToSolve(grid) => {
      model.setGrid(grid)
      SudokuView.displaySolution(model.solve())
    }
  }
}