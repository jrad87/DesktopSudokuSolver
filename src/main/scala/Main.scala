import SudokuController._
import SudokuModel._

object Main extends App {
  val model: SudokuModel = new SudokuModel()
  val controller: SudokuController = new SudokuController(model)
  controller.startup(args)
}