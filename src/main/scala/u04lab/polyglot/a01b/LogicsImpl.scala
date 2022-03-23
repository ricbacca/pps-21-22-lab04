package u04lab.polyglot.a01b
import u04lab.polyglot.{OptionToOptional, Pair}
import u04lab.code.Option
import u04lab.code.Option.*
import u04lab.code.List
import u04lab.code.List.*
import u04lab.code.Stream.*
import scala.util.Random

class LogicsImpl(private val gridSize: Int, private val mines: Int) extends Logics:
  var minesSet:List[Pair[Int, Int]] = Nil()
  var selected:List[Pair[Int, Int]] = Nil()

  def getRandomMine():Pair[Int, Int] =
    var pair = Pair[Int, Int](Random.nextInt(gridSize), Random.nextInt(gridSize))
    while(List.contains(minesSet, pair)) pair = Pair[Int, Int](Random.nextInt(gridSize), Random.nextInt(gridSize))
    pair

  def appendMines(): Unit =
    (0 until mines).foreach(i => minesSet =
      List.append(minesSet, Cons(getRandomMine(), List.Nil())))

  def appendGridPoint(x: Int, y:Int): Unit = selected = List.append(selected, Cons(Pair[Int, Int](x, y), List.Nil()))

  def hit(x: Int, y: Int): java.util.Optional[Integer] =
    if minesSet == Nil() then appendMines()
    if List.contains(minesSet, Pair[Int, Int](x, y)) then OptionToOptional(None()) else
      appendGridPoint(x, y)
      OptionToOptional(neighbours(x, y))

  def neighbours(x: Int, y: Int): Option[Int] =
    var counter = 0;
    (x-1 to x+1 by +1).foreach(i => (y-1 to y+1 by +1).foreach(j =>
      if List.contains(minesSet, Pair[Int, Int](i, j)) then counter = counter + 1))
    Some(counter)

  def won: Boolean = length(selected) + length(minesSet) == gridSize * gridSize;
