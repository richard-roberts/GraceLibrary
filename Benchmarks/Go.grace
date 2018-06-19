import "random" as random

def rand = random.Jenkins(1.asInteger)

def size = 9
def games = 200
def komi = 7.5
def empty = 0
def white = 1
def black = 2
def pass = -1
def maxmoves = size * size * 3

var globalTimestamp := 0
var globalMoves := 0

method toPos(x, y) {
  y * size + x
}

method toXY(pos) {
  def y = (pos / size).asInteger
  def x = (pos % size)
  [y, x]
}

class Square(board, pos) {
  var timestamp := globalTimestamp
  var removestamp := globalTimestamp
  def zobristStrings = [rand.next, rand.next, rand.next]
  var neighbours := Done
  var color := 0
  var reference := Done
  var ledges := 0
  var used := false


  method setNeighbours {
    def x = pos % size
    def y = pos / size

    neighbours := platform.kernel.Vector.new()
    [ [ -1, 0 ], [ 1, 0 ], [ 0, -1 ], [ 0, 1] ].do { d ->
      def dx = d.at(1)
      def dy = d.at(2)
      def newX = x + dx
      def newY = y + dy
      if ((0 <= newX) && (newX < size) && (0 <= newY) && (newY < size)) then {
        neighbours.append(board.squares.at(toPos(newX, newY)))
      }
    }
  }

  method move(color') {
    globalTimestamp := globalTimestamp + 1
    globalMoves := globalMoves + 1

    board.zobrist.update(self, color')
    color := color'
    reference := self
    ledges := 0

    neighbours.do { neighbour ->
      def neighcolor = neighbour.color
      if (neighbour == empty)
      then { ledges := ledges + 1 }
      else {
        def neighbourRef = neighbour.find(true)
        if (neighbour == color) then {
          if (neighbourRef.reference.pos != pos) then {
            ledges := ledges + neighbourRef.ledges
            neighbourRef.reference := self
          }
          ledges := ledges - 1
        } else {
          neighbourRef.ledges := neighbourRef.ledges - 1
          if (neighbourRef.ledges == 0) then {
            neighbour.remove(neighbourRef)
          }
        }
      }
    }
    board.zobrist.add
  }

  method remove(reference, update) {
    board.zobrist.update(self, empty)
    removestamp := globalTimestamp
    if (update) then {
      color := empty
      board.emptyset.add(pos)
    }

    neighbours.do { neighbour ->
      if ((neighbour.color != empty) &&
          (neighbour.removestamp != globalTimestamp)) then {
        neighbourRef := neighbour.find(update)
        if (neighbourRef.pos == reference.pos) then {
          neighbour.remove(reference, update)
        } else {
          if (update) then {
            neighbourRef.ledges := neighbourRef.ledges + 1
          }
        }
      }
    }
  }

  method find(update) {
    var reference' := reference
    if (reference'.pos != pos) then {
      reference' := reference'.find(update)
      if (update) then {
        reference := reference'
      }
    }
    return reference'
  }
}

class EmptySet(board) {
  def empties = platform.kernel.Vector.new(size * size)
  def emptyPos = platform.kernel.Vector.new(size * size)

  method randomChoice {
    def choices = empties.size
    {choices}.whileTrue {
      def i = rand.next % choices
      def pos = empties.at(i.asInteger)
      if (board.useful(pos)) then {
        return pos
      }
      choices := choices - 1
      set(i, empties.at(choices))
      set(choices, pos)
    }
    return pass
  }

  method add(pos) {
    emptyPos.at(pos)put(empties.size)
    empties.append(pos)
  }

  method remove(pos) {
    set(emptyPos.at(pos), empties.at(empties.size))
    empties.remove
  }

  method set(i, pos) {
    empties.at(i)put(pos)
    emptyPos.at(pos)put(i)
  }
}

class ZobristHash(board) {
  def hashSet = kernel.collections.Set.new
  var hash := 0
  board.squares.do { square ->
    hash := hash.bitXor(square.zobristStrings.at(empty))
  }
  hashSet.clear
  hashSet.add(hash)

  method update(square, color) {
    hash := hash.bitXor(square.zobristStrings.at(square.color))
    hash := hash.bitXor(square.zobristStrings.at(color))
  }

  method add {
    hashSet.add(hash)
  }

  method dupe {
    return hashSet.contains(hash)
  }
}

