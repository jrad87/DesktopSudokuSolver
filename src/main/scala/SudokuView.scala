package SudokuView

import scala.swing.{Alignment, _}
import event._

object SudokuView extends Publisher {

  case class PuzzleSolved(grid: Array[Int]) extends event.Event
  case class TimeToSolve(grid: Array[Int]) extends Event

  def updateViewModel(id: Int, value: Int) = {
    SudokuViewModel.data(id) = value
  }

  //
  def displaySolution(grid: Array[Array[Int]]): Unit = {
    SudokuViewModel.updateGridAfterSolved(grid)
  }

  object SolveButton extends Button {
    this.text = "Solve It"
    this.reactions += {
      case (e: ButtonClicked) => publishButtonClick()
    }
  }

  def publishButtonClick() = {
    publish(TimeToSolve(SudokuViewModel.data))
  }

  object SudokuViewModel extends Publisher {
    var data = Array.fill[Int](81)(0)
    override def toString = data.mkString(", ")
    def updateGridAfterSolved(grid: Array[Array[Int]]): Unit = {
      data = grid.flatten
      publish(PuzzleSolved(data))
    }
  }

  class FiniteDigitField(id: Int) extends TextField {
    // listen for event.KeyTyped
    listenTo(keys)
    // listen for event.EditDone
    listenTo(this)
    // listen for PuzzleSolved
    listenTo(SudokuViewModel)
    columns = 1
    super.preferredSize_=(new swing.Dimension(30,30))
    super.horizontalAlignment_=(Alignment.Center)

    font = new Font("Terminal", 1 ,30)

    def validator(e : event.KeyTyped): Boolean = {
      return (text.length < 1 && e.char.isDigit)
    }
    reactions += {
      case (e : event.KeyTyped) => {
        if(!validator(e)) e.consume
        requestFocusInWindow()
      }
      case e: event.EditDone => {
        if(this.text != "") updateViewModel(id, Integer.parseInt(this.text))
      }
      case PuzzleSolved(grid) => {
        this.text = grid(id).toString
      }
    }
  }

  class SudokuSector(val rowRange: Int, val colRange: Int) extends GridBagPanel {
    def constraints(x: Int, y: Int,
                    gridwidth: Int = 1, gridheight: Int = 1,
                    weightx: Double = 0.0, weighty: Double = 0.0,
                    fill: GridBagPanel.Fill.Value = GridBagPanel.Fill.None,
                    ipadx:Int = 0, ipady: Int = 0)
    : Constraints = {
      val c = new Constraints
      c.gridx = x
      c.gridy = y
      c.gridwidth = gridwidth
      c.gridheight = gridheight
      c.weightx = weightx
      c.weighty = weighty
      c.fill = fill
      c.ipadx = ipadx
      c.ipady = ipady
      c
    }
    (for(row <- 0 until 3; column <- 0 until 3) yield (row, column)).foreach(pair => {
      val id: Int = (rowRange * 3 + pair._2) * 9 + (colRange * 3 + pair._1)
      val field = new FiniteDigitField(id)
      add(field , constraints(pair._1, pair._2))
    })
  }
  val main = new MainFrame {
    title = "Sudoku Solver"
    resizable = false
    contents = new GridBagPanel() {
      def constraints(x: Int, y: Int,
                      gridwidth: Int = 1, gridheight: Int = 1,
                      weightx: Double = 0.0, weighty: Double = 0.0,
                      fill: GridBagPanel.Fill.Value = GridBagPanel.Fill.None,
                      ipadx:Int = 0, ipady: Int = 0)
      : Constraints = {
        val c = new Constraints
        c.gridx = x
        c.gridy = y
        c.gridwidth = gridwidth
        c.gridheight = gridheight
        c.weightx = weightx
        c.weighty = weighty
        c.fill = fill
        c.ipadx = ipadx
        c.ipady = ipady
        c
      }
      add(new SudokuSector(0, 0), constraints(0,0, ipadx=10, ipady=10))
      add(new SudokuSector(0, 1), constraints(1,0, ipadx=10, ipady=10))
      add(new SudokuSector(0, 2), constraints(2,0, ipadx=10, ipady=10))
      add(new SudokuSector(1, 0), constraints(0,1, ipadx=10, ipady=10))
      add(new SudokuSector(1,1), constraints(1,1, ipadx=10, ipady=10))
      add(new SudokuSector(1, 2), constraints(2,1, ipadx=10, ipady=10))
      add(new SudokuSector(2, 0), constraints(0,2, ipadx=10, ipady=10))
      add(new SudokuSector(2, 1), constraints(1,2, ipadx=10, ipady=10))
      add(new SudokuSector(2, 2), constraints(2,2, ipadx=10, ipady=10))
      add(SolveButton, constraints(0, 3, fill = GridBagPanel.Fill.Both))

    }
  }
}