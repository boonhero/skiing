package com.example

import scala.util.{Failure, Try}

/**
 * Movement result
 * OK - pathway accessible
 * NOT_OK - condition failed (lower than the number or the path is already marked)
 * LIMIT - when you are already at the edge of the grid.
 */
object MovementResult extends Enumeration {
  type MovementResult = Value
  val OK, NOT_OK, LIMIT = Value
}

/**
 * Grid board
 */
object Input {
  var grid: Array[Array[Int]] = Array[Array[Int]]()
}

/**
 * Controls the your movement
 */
class Control {
  // current position
  private var pointer = (0, 0)

  // current position value
  private var currentNumber: Int = 0;

  // passed thru pointers are put here in format (x-y)
  private var marks: Vector[String] = Vector[String]()


  // deep copy this control
  def copy() : Control = {
    val control = new Control()
    control.setPointer(pointer._1, pointer._2)
    control.setCurrentNumber(this.currentNumber)
    control.setMarks(this.marks)
    control
  }

  def getPointer() : (Int, Int) = pointer
  def setPointer(x: Int, y: Int): Unit = {
    pointer = (x, y)
    currentNumber = Input.grid(pointer._1)(pointer._2)
    marks = marks :+ s"${pointer._1}-${pointer._2}"
//    println(s"setPointer x: ${x} y: ${y} val: ${getCurrentNumber()}")
  }
  def getCurrentNumber() = currentNumber
  def setCurrentNumber(currentNumber: Int): Unit = {
    this.currentNumber = currentNumber
  }
  def getMarks() = marks
  def setMarks(marks: Vector[String]): Unit = {
    this.marks = marks
  }

  /**
   * Determines which paths are accessible
   * @return List of MovementResults
   */
  def checkAllRoutes() = {
    List(checkNorth(), checkSouth(), checkWest(), checkEast()).zipWithIndex
  }

  def checkNorth() = {
    val x = pointer._1

    if (x == 0) {
      MovementResult.LIMIT
    } else {
      check(x - 1, pointer._2) match {
        case Some(num) if (num < currentNumber) => {
          MovementResult.OK
        }
        case _ => MovementResult.NOT_OK
      }

    }
  }

  def checkSouth() = {
    val x = pointer._1

    if (x == Input.grid.length - 1) {
      MovementResult.LIMIT
    } else {

      check(x + 1, pointer._2) match {
        case Some(num) if (num < currentNumber) => {
          MovementResult.OK
        }
        case _ => MovementResult.NOT_OK
      }

    }
  }

  def checkWest() = {
    val y = pointer._2

    if (y == 0) {
      MovementResult.LIMIT
    } else {

      check(pointer._1, y - 1) match {
        case Some(num) if (num < currentNumber) => {
          MovementResult.OK
        }
        case _ => MovementResult.NOT_OK
      }

    }
  }

  def checkEast() = {
    val y = pointer._2

    if (y == Input.grid(pointer._1).length - 1) {
      MovementResult.LIMIT
    } else {
      check(pointer._1, y + 1) match {
        case Some(num) if (num < currentNumber) => {
          MovementResult.OK
        }
        case _ => MovementResult.NOT_OK
      }
    }
  }

  def goNorth() = setPointer(pointer._1 - 1, pointer._2)
  def goSouth() = setPointer(pointer._1 + 1, pointer._2)
  def goWest() = setPointer(pointer._1, pointer._2 - 1)
  def goEast() = setPointer(pointer._1, pointer._2 + 1)

  private def check(x: Int, y: Int): Option[Int] = {
//    println(s"check x: ${x} y:${y}")
    val num = Input.grid(x)(y)
    if (!marks.contains(s"${x}-${y}")) {
      Some(num)
    } else {
      None
    }
  }

  override def toString = s"Control($pointer, $currentNumber, $marks)"
}

object Control {
  var control: Control = Control()

  def apply() = {
    control = new Control()
    (control.setPointer _).tupled(control.pointer)
    control
  }

  def apply(x: Int, y: Int) = {
    control = new Control()
    (control.setPointer _).tupled((x, y))
    control
  }
}
