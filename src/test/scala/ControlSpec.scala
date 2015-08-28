import com.example.{Input, MovementResult, Control}
import com.sun.xml.internal.bind.v2.TODO
import org.scalatest._

class ControlSpec extends FlatSpec with Matchers with BeforeAndAfter {

  before {

  }

  behavior of "a Control North"

  it should "LIMIT it self when at top" in {
    Input.grid = Array(Array(1, 2, 3))
    val control: Control = Control()
    val north: MovementResult.Value = control.checkNorth()
    assert(north == MovementResult.LIMIT)
  }

  it should "return OK when top is lower number" in {
    Input.grid = Array(Array(0, 0, 0), Array(1, 1, 1))
    val control: Control = Control(1, 0)
    val north: MovementResult.Value = control.checkNorth()
    assert(north == MovementResult.OK)
    assert(control.getCurrentNumber == 1)
  }

  it should "return NOT_OK when top is higher number" in {
    Input.grid = Array(Array(1, 1, 1), Array(0, 0, 0))
    val control: Control = Control(1, 0)
    assert(control.getCurrentNumber == 0)
    val north: MovementResult.Value = control.checkNorth()
    assert(north == MovementResult.NOT_OK)
    assert(control.getCurrentNumber == 0)
  }

  behavior of "a Control South"

  it should "LIMIT it self when at bottom" in {
    Input.grid = Array(Array(1, 2, 3))
    val control: Control = Control()
    val south: MovementResult.Value = control.checkSouth()
    assert(south == MovementResult.LIMIT)
  }

  it should "return OK when bottom is lower number" in {
    Input.grid = Array(Array(1, 1, 1), Array(0, 0, 0))
    val control: Control = Control(0, 0)
    val south: MovementResult.Value = control.checkSouth()
    assert(south == MovementResult.OK)
    assert(control.getCurrentNumber == 1)
  }

  it should "return NOT_OK when bottom is higher number" in {
    Input.grid = Array(Array(0, 0, 0), Array(1, 1, 1))
    val control: Control = Control(0, 0)
    val south: MovementResult.Value = control.checkSouth()
    assert(south == MovementResult.NOT_OK)
    assert(control.getCurrentNumber == 0)
  }

  behavior of "a Control West"

  it should "LIMIT it self when at left most" in {
    Input.grid = Array(Array(1, 2, 3))
    val control: Control = Control()
    val west: MovementResult.Value = control.checkWest()
    assert(west == MovementResult.LIMIT)
  }

  it should "return OK when left is lower number" in {
    Input.grid = Array(Array(1, 2, 3))
    val control: Control = Control(0, 1)
    val west: MovementResult.Value = control.checkWest()
    assert(west == MovementResult.OK)
    assert(control.getCurrentNumber == 2)
  }

  it should "return NOT_OK when left is higher number" in {
    Input.grid = Array(Array(2, 1, 3))
    val control: Control = Control(0, 1)
    val west: MovementResult.Value = control.checkWest()
    assert(west == MovementResult.NOT_OK)
    assert(control.getCurrentNumber == 1)
  }

  behavior of "a Control East"

  it should "LIMIT it self when at right most" in {
    Input.grid = Array(Array(1, 2, 3))
    val control: Control = Control(0, 2)
    val east: MovementResult.Value = control.checkEast()
    assert(east == MovementResult.LIMIT)
  }

  it should "return OK when right is lower number" in {
    Input.grid = Array(Array(1, 2, 1))
    val control: Control = Control(0, 1)
    val east: MovementResult.Value = control.checkEast()
    assert(east == MovementResult.OK)
    assert(control.getCurrentNumber == 2)
  }

  it should "return NOT_OK when right is higher number" in {
    Input.grid = Array(Array(2, 1, 3))
    val control: Control = Control(0, 1)
    val east: MovementResult.Value = control.checkEast()
    assert(east == MovementResult.NOT_OK)
    assert(control.getCurrentNumber == 1)
  }

  behavior of "movement"

  it should "go north" in {
    Input.grid = Array(Array(0), Array(1))
    val control: Control = Control(1, 0)
    assert(control.getCurrentNumber() == 1)
    control.goNorth()
    assert(control.getCurrentNumber() == 0)
    assert(control.getMarks().size == 2)
  }
  it should "go south" in {
    Input.grid = Array(Array(0), Array(1))
    val control: Control = Control(0, 0)
    assert(control.getCurrentNumber() == 0)
    control.goSouth()
    assert(control.getCurrentNumber() == 1)
    assert(control.getMarks().size == 2)
  }
  it should "go west" in {
    Input.grid = Array(Array(0, 1))
    val control: Control = Control(0, 1)
    assert(control.getCurrentNumber() == 1)
    control.goWest()
    assert(control.getCurrentNumber() == 0)
    assert(control.getMarks().size == 2)
  }
  it should "go east" in {
    Input.grid = Array(Array(0, 1))
    val control: Control = Control(0, 0)
    assert(control.getCurrentNumber() == 0)
    control.goEast()
    assert(control.getCurrentNumber() == 1)
    assert(control.getMarks().size == 2)
  }
}
