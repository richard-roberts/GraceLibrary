import "random" as random

var BOARDHEIGHT := 20.asInteger
var BOARDWIDTH := 30.asInteger

method resetRandom -> Done {
  random.setSeed(1324.asInteger)
  Done
}


type Position = interface {
  x
  y
}

class Position(x: Number, y: Number) -> Position {
  method asString -> String {
    return "({x}, {y})"
  }
}

method randomPosition -> Position {
   Position ( random.randomBetween(1.asInteger)and(BOARDWIDTH - 1.asInteger),
              random.randomBetween(1.asInteger)and(BOARDHEIGHT - 1.asInteger) )
}

method samePosition(a: Position, b: Position) -> Boolean {
  (a.x == b.x) && (a.y == b.y)
}

type Snake = {
  segments
  direction
  head
  collidedWithWall
  collidedWithSelf
  nextHead
  slither
  grow
  asString
}

class Snake(segments: List)  {
  var direction: String

  method head -> Position {
    return segments.at(segments.size)
  }

  method collidedWithWall -> Boolean {
    (head.x <= 0) || (head.x >= BOARDWIDTH) || (head.y <= 0) || (head.y >= BOARDHEIGHT)
  }

  method collidedWithSelf -> Boolean {
    1.asInteger.to(segments.size) do { i: Number ->
      (i + 1.asInteger).to(segments.size) do { j: Number ->
        (samePosition(segments.at(i), segments.at(j))).ifTrue {
          return true
        }
      }
    }
    return false
  }

  method nextHead -> Position {
    ("right" == direction). ifTrue { return Position(head.x + 1.asInteger, head.y              ) }
    ("left"  == direction). ifTrue { return Position(head.x - 1.asInteger, head.y              ) }
    ("down"  == direction). ifTrue { return Position(head.x,               head.y - 1.asInteger) }
    ("up"    == direction). ifTrue { return Position(head.x,               head.y + 1.asInteger) }
    error("{direction} not understood as a direction?")
  }

  method slither -> Done {
    segments.append(nextHead)
    segments.remove(segments.at(1.asInteger))
    Done
  }

  method grow -> Done {
    segments.append(nextHead)
    Done
  }

  method isTouching(position: Position) -> Boolean {
    segments.do { seg ->
      samePosition(seg, position).ifTrue { return true }
    }
    return false
  }

  method asString -> String {
    var s := "Snake\n  segs={segments.size}\n"
    segments.do { seg ->
      s := "{s}  {seg}\n"
    }
    return s
  }
}

type World = interface {
  food
  snake
  isGameOver
  tick
}

class World {

  var snake: Snake
  var food: Position
  var moves: Number := 0.asInteger

  method reset {
    var segments: List := platform.kernel.Vector.new
    segments.append(Position(10.asInteger, 15.asInteger))
    snake := Snake(segments)
    snake.direction := "right"
    food := randomPosition
    moves := 0.asInteger
  }

  method isGameOver -> Boolean {
    snake.collidedWithWall || snake.collidedWithSelf
  }  

  method tick {
    samePosition(food, snake.head).ifTrue {
      snake.grow
      food := randomPosition
    } ifFalse {
      snake.slither
    }

    moves := moves + 1.asInteger
  }

  method handleKey (key: String) -> Done {
    (key == "w"). ifTrue { 
      snake.direction := "up"
      return Done
    }
    (key == "s"). ifTrue { 
      snake.direction := "down"
      return Done
    }
    (key == "a"). ifTrue { 
      snake.direction := "left"
      return Done
    }
    (key == "d"). ifTrue {
      snake.direction := "right"
      return Done
    }

    error("{key} not understood as a key?")
  }

  method render {
    var renderStr := ""

    0.asInteger.to(BOARDHEIGHT) do { y ->
      var rowStr := ""

      0.asInteger.to(BOARDWIDTH) do { x ->
        var p := Position(x, y)

        var isWall := (x <= 0) || (x >= BOARDWIDTH) || (y <= 0) || (y >= BOARDHEIGHT)
        var isSnake := snake.isTouching(p)
        var isFood := samePosition(food, p)

        isSnake.ifTrue { rowStr := rowStr ++ "S" } ifFalse {
          isFood.ifTrue { rowStr := rowStr ++ "O" } ifFalse {
            isWall.ifTrue { rowStr := rowStr ++ "X" } ifFalse {
              rowStr := rowStr ++ " "
            }
          }
        }
      }

      renderStr := "{rowStr}\n" + renderStr
    }

    print(renderStr)
  }

}

method replay (world: World, history: List) -> Done {
  resetRandom
  world.reset

  history.do { item ->

    (item == "t").ifTrue {
      world.tick
      // world.render
      world.isGameOver.ifTrue {
        return Done
      }
    } ifFalse {
      world.handleKey(item)
    }
  }

  Done
}

method asString -> String {
  "Snake.grace"
}

method benchmark(innerIterations: Number) -> Done {
  def world: World = World
  def history = [
    "t", "t", "t", "t", "t", "t", "t", "t", "t", "t",
    "s", "t", "t", "t", "d", "t", "t", "t",
    "w", "t", "t", "t", "t", "t", "t",
    "a", "t", "t", "t", "t", "t", "t", "t",
    "s", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t",
    "a", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t",
    "w", "t", "t", "t", "t",
    "d", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t",
    "w", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t",
    "a", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t",
    "s", "t", "t", "t",
    "d", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t",
    "w", "t", "t", "t", "t", "t", "t",
    "a", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t",
    "s", "t", "t", "t", "t", "t", "t", "t", "t", "t", "t",
    "a", "t", "t",
    "w", "t", "t",
    "d", "t", "t", "t", "t", "t", "t"
  ]
  
  1.asInteger.to(innerIterations) do { i ->
    replay(world, history)

    (world.moves != 157.asInteger).ifTrue {
      error("{self} failed, {result} != true")
    }
    
    (world.snake.segments.size != 10.asInteger).ifTrue {
      error("{self} failed, {result} != true")
    }

    (world.isGameOver).ifFalse {
      error("{self} failed, game is not over")
    }
  }

  Done
}
