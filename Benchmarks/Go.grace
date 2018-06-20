import "random" as random

def size = 9.asInteger
def komi = 7.5
def empty = 1.asInteger
def white = 2.asInteger
def black = 3.asInteger
def pass = -1.asInteger
def maxmoves = size * size * 3

var globalTimestamp := 0
var globalMoves := 0

method toPos(x, y) {
  y * size + x
}

method toXY(pos) {
  def y = (pos / size).asInteger
  def x = (pos % size).asInteger
  [y, x]
}

class Square(board, pos) {
  var timestamp := globalTimestamp
  var removestamp := globalTimestamp
  def zobristStrings = [random.next, random.next, random.next]
  var neighbours := Done
  var color := 0.asInteger
  var reference := Done
  var ledges := 0.asInteger
  var used := false
  var tempLedges := 0.asInteger


  method setNeighbours {
    def x = pos % size
    def y = pos / size

    neighbours := platform.kernel.Vector.new()
    [ [ -1, 0 ], [ 1, 0 ], [ 0, -1 ], [ 0, 1 ] ].do { d ->
      def dx = d.at(1.asInteger)
      def dy = d.at(2.asInteger)
      def newX = x + dx
      def newY = y + dy
      ((0 <= newX) && (newX < size) && (0 <= newY) && (newY < size)).ifTrue {
        neighbours.append(
          board.squares.at(toPos(newX.asInteger, newY.asInteger) + 1.asInteger))
      }
    }
  }

  method move(color') {
    globalTimestamp := globalTimestamp + 1.asInteger
    globalMoves := globalMoves + 1.asInteger

    board.zobrist.update(self, color')
    color := color'
    reference := self
    ledges := 0.asInteger
    used := true

    neighbours.do { neighbour ->
      def neighcolor = neighbour.color
      // print("S.move nc: " + (neighcolor - 1.asInteger) + " ledges: " + ledges)
      (neighcolor == empty).ifTrue {
        ledges := ledges + 1.asInteger
      } ifFalse {
        def neighbourRef = neighbour.find(true)
        // print("found ref: " + neighbourRef.pos)
        (neighcolor == color').ifTrue {
          (neighbourRef.reference.pos != pos).ifTrue {
            ledges := ledges + neighbourRef.ledges
            neighbourRef.reference := self
          }
          ledges := ledges - 1.asInteger
          // print("ledges: " + ledges)
        } ifFalse {
          neighbourRef.ledges := neighbourRef.ledges - 1.asInteger
          (neighbourRef.ledges == 0.asInteger).ifTrue {
            // print("ledges == 0")
            neighbour.remove(neighbourRef, true)
          }
        }
      }
    }
    board.zobrist.add
  }

  method remove(reference, update) {
    board.zobrist.update(self, empty)
    removestamp := globalTimestamp
    (update).ifTrue {
      color := empty
      // print("add empty " + pos)
      board.emptyset.add(pos)
    }

    neighbours.do { neighbour ->
      ((neighbour.color != empty) &&
          (neighbour.removestamp != globalTimestamp)).ifTrue {
        def neighbourRef = neighbour.find(update)
        (neighbourRef.pos == reference.pos).ifTrue {
          neighbour.remove(reference, update)
        } ifFalse {
          update.ifTrue {
            neighbourRef.ledges := neighbourRef.ledges + 1.asInteger
          }
        }
      }
    }
  }

  method find(update) {
    var reference' := reference
    (reference'.pos != pos).ifTrue {
      reference' := reference'.find(update)
      (update).ifTrue {
        reference := reference'
      }
    }
    return reference'
  }
}

class EmptySet(board) {
  def empties = platform.kernel.Vector.new(size * size)
  def emptyPos = platform.kernel.Vector.new(size * size)
  empties.appendAll(0.asInteger.to((size * size) - 1.asInteger))
  emptyPos.appendAll(0.asInteger.to((size * size) - 1.asInteger))

  method randomChoice {
    def choices = empties.size
    {choices > 0.asInteger}.whileTrue {
      // print("choices " + choices)
      def i = (random.next % choices)
      def pos = empties.at(i + 1.asInteger)
      (board.useful(pos)).ifTrue {
        // print("randomChoice useful " + pos)
        return pos
      }
      // print("randomChoice not useful")
      choices := choices - 1.asInteger
      set(i, empties.at(choices + 1.asInteger))
      set(choices, pos)
    }
    return pass
  }

  method add(pos) {
    emptyPos.at(pos + 1.asInteger)put(empties.size)
    empties.append(pos)
  }

  method remove(pos) {
    // print("emptyPos.size: " + emptyPos.size)
    set(emptyPos.at(pos + 1.asInteger), empties.at(empties.size))
    empties.remove
  }

  method set(i, pos) {
    empties.at(i + 1.asInteger)put(pos)
    emptyPos.at(pos + 1.asInteger)put(i)
  }
}

class ZobristHash(board) {
  def hashSet = platform.collections.Set.new
  var hash := 0.asInteger
  board.squares.do { square ->
    hash := hash.bitXor(square.zobristStrings.at(empty))
  }
  hashSet.removeAll
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

class Board {
  var emptyset := Done
  var zobrist := Done
  var color := empty
  var finished := false
  var lastmove := -2.asInteger
  var history := Done
  var whiteDead := 0.asInteger
  var blackDead := 0.asInteger

  def squares = platform.kernel.Array.new(size * size)
  1.asInteger.to(size * size).do { pos ->
    // print(pos)
    squares.at(pos)put(Square(self, (pos - 1.asInteger)))
  }
  squares.do { square -> square.setNeighbours }
  reset()

  method reset {
    squares.do { square ->
      square.color := empty
      square.used := false
    }
    emptyset := EmptySet(self)
    zobrist := ZobristHash(self)
    color := black
    finished := false
    lastmove := -2.asInteger
    history := platform.kernel.Vector.new
    whiteDead := 0.asInteger
    blackDead := 0.asInteger
  }

  method move(pos) {
    // print("B.move: " + pos)
    (pos != pass).ifTrue {
      var square' := squares.at(pos + 1.asInteger)
      // print("B.move square: " + square'.pos)
      square'.move(color)
      emptyset.remove(square'.pos)
    } ifFalse {
      // print("lastmove: " + lastmove)
      (lastmove == pass).ifTrue {
        // print("set finished")
        finished := true
      }
    }

    (color == black).ifTrue {
      color := white
    } ifFalse {
      color := black
    }
    lastmove := pos
    history.append(pos)
  }

  method randomMove {
    emptyset.randomChoice
  }

  method usefulFast(square) {
    // print(square.used)
    (!square.used).ifTrue {
      // print("square.neighbours.size")
      // print(square.neighbours.size)
      square.neighbours.do { neighbour ->
        // print(neighbour.color)
        (neighbour.color == empty).ifTrue {
          return true
        }
      }
    }
    return false
  }

  method useful(pos) {
    globalTimestamp := globalTimestamp + 1.asInteger
    var square := squares.at(pos + 1.asInteger)
    (usefulFast(square)).ifTrue {
      return true
    }

    // print("useful: not fast")
    def oldHash = zobrist.hash
    zobrist.update(square, color)
    var empties    := 0.asInteger
    var opps       := 0.asInteger
    var weakOpps   := 0.asInteger
    var neighs     := 0.asInteger
    var weakNeighs := 0.asInteger

    square.neighbours.do { neighbour ->
      def neighcolor = neighbour.color
      (neighcolor == empty).ifTrue {
        empties := empties + 1.asInteger
      } ifFalse {
        def neighbourRef = neighbour.find(false)
        (neighbourRef.timestamp != globalTimestamp).ifTrue {
          (neighcolor == color).ifTrue {
            neighs := neighs + 1.asInteger
          } ifFalse {
            opps := opps + 1.asInteger
          }
          neighbourRef.timestamp := globalTimestamp
          neighbourRef.tempLedges := neighbourRef.ledges
        }
        neighbourRef.tempLedges := neighbourRef.tempLedges - 1.asInteger
        (neighbourRef.tempLedges == 0).ifTrue {
          (neighcolor == color).ifTrue {
            weakNeighs := weakNeighs + 1.asInteger
          } ifFalse {
            weakOpps := weakOpps + 1.asInteger
            neighbourRef.remove(neighbourRef, false)
          }
        }
      }
    }
    def dupe = zobrist.dupe()
    zobrist.hash := oldHash
    def strongNeighs = neighs - weakNeighs
    def strongOpps = opps - weakOpps
    // print("return: ")
    // print(!dupe && ((empties != 0) || (weakOpps != 0) || (
    //  (strongNeighs != 0) && ((strongOpps != 0) || (weakNeighs != 0)))))
    return !dupe && ((empties != 0) || (weakOpps != 0) || (
      (strongNeighs != 0) && ((strongOpps != 0) || (weakNeighs != 0))))
  }

  method usefulMoves {
    // print("usefulMoves")
    // print(emptyset.empties.size)
    return emptyset.empties.select { pos -> useful(pos) }
  }

  method replay(history) {
    // print("Replay: " + history.size)
    history.do { pos -> move(pos) }
  }

  method score(color) {
    var count
    (color == white).ifTrue {
      count := komi + blackDead
    } ifFalse {
      count := whiteDead
    }
    squares.do { square ->
      def squarecolor = square.color
      (squarecolor == color).ifTrue {
        count := count + 1.asInteger
      } ifFalse {
        (squarecolor == empty).ifTrue {
          var surround := 0
          square.neighbours.do { neighbour ->
            (neighbour.color == color).ifTrue {
              surround := surround + 1.asInteger
            }
          }
          (surround == square.neighbours.size).ifTrue {
            count := count + 1.asInteger
          }
        }
      }
    }
    return count
  }

  method check {
    squares.do { square ->
      if (square.color != empty) then {
        def members1 = platform.collections.Set.new
        members1.add(square)

        var changed := true
        { changed }.whileTrue {
          changed := false
          def copy = platform.collections.Set.new
          copy.addAll(members1)
          copy.do { member ->
            member.neighbours.do { neighbour ->
              if ((neighbour.color == square.color) && !members1.contains(neighbour)) then {
                changed := true
                members1.add(neighbour)
              }
            }
          }
        }

        var ledges1 := 0
        members1.do { member ->
          member.neighbours.do { neighbour ->
            if (neighbour.color == EMPTY) then {
              ledges1 := ledges1 + 1.asInteger
            }
          }
        }

        def root = square.find()

        // print 'members1', square, root, members1
        // print 'ledges1', square, ledges1

        def members2 = platform.collections.Set.new
        squares.do { square2 ->
          if ((square2.color != empty) && (square2.find() == root)) then {
            members2.add(square2)
          }
        }

        def ledges2 = root.ledges
        // print 'members2', square, root, members1
        // print 'ledges2', square, ledges2

        def size1 = members1.size
        members1.addAll(members2)
        if (size1 != members1.size) then {
          error("members1 and members2 do not contain the same elements")
        }
        if (ledges1 != ledges2) then {
          error("ledges differ at " + square + " " + ledges1 + " " + ledges2)
        }
      }
    }
  }

  // def __repr__(self):
  //     result = []
  //     for y in range(SIZE):
  //         start = to_pos(0, y)
  //         result.append(''.join(
  //             [SHOW[square.color] + ' ' for square in self.squares[start:start + SIZE]]))
  //     return '\n'.join(result)
}

class UCTNode {

  var bestchild := Done
  var pos := -1
  var wins := 0
  var losses := 0
  var parent := Done
  def posChild = platform.kernel.Array.new(size * size)
  var unexplored := Done

  method playLoop(board, node', path) {
    var node := node'
    true.whileTrue {
      def pos = node.select()
      (pos == pass).ifTrue {
        return
      }
      board.move(pos)
      var child := node.posChild.at(pos + 1.asInteger)
      (child == Done).ifTrue {
        child := UCTNode()
        node.posChild.at(pos + 1.asInteger)put(child)
        child.unexplored := board.usefulMoves()
        child.pos := pos
        child.parent := node
        path.append(child)
        return
      }
      path.append(child)
      node := child
    }
  }

  method play(board) {
    // uct tree search
    def color = board.color
    def node = self
    def path = platform.kernel.Vector.with(node)

    playLoop(board, node, path)

    randomPlayout(board)
    updatePath(board, color, path)
  }

  method select() {
    // select move; unexplored children first, then according to uct value
    ((unexplored ~= Done) && (!unexplored.isEmpty)).ifTrue {
        // print("unexplored.size " + unexplored.size)
        def i = (random.next % unexplored.size) + 1.asInteger
        def pos = unexplored.at(i)
        unexplored.at(i)put(unexplored.at(unexplored.size))
        unexplored.remove()
        return pos
    } ifFalse {
      (bestchild ~= Done).ifTrue {
        return bestchild.pos
      } ifFalse {
        return pass
      }
    }
  }

  method randomPlayout(board) {
    // random play until both players pass
    // XXX while not self.finished?
    1.asInteger.to(maxmoves.asInteger)do { i ->
      board.finished.ifTrue {
        // print("random_playout finished")
        return
      }
      board.move(board.randomMove())
    }
  }

  method updatePath(board, color', path) {
    var color := color'
    // update win/loss count along path
    def wins = board.score(black) >= board.score(white)
    path.do { node ->
      (color == black).ifTrue {
          color := white
      } ifFalse {
          color := black
      }

      (wins == (color == black)).ifTrue {
        node.wins := node.wins + 1.asInteger
      } ifFalse {
        node.losses := node.losses + 1.asInteger
      }

      (node.parent == Done).ifFalse {
        node.parent.bestchild := node.parent.bestChild()
      }
    }
  }

  method score {
    def winrate = wins / (wins + losses)
    def parentvisits = parent.wins + parent.losses
    (parentvisits == 0).ifTrue {
      return winrate
    }
    def nodevisits = wins + losses
    return winrate + (parentvisits.log / (5 * nodevisits)).sqrt
  }

  method bestChild {
    var maxscore := -1.asInteger
    var maxchild := Done
    posChild.do { child ->
      (child == Done).ifFalse {
        (child.score() > maxscore).ifTrue {
          maxchild := child
          maxscore := child.score()
        }
      }
    }
    return maxchild
  }

  method bestVisited {
    var maxvisits := -1
    var maxchild := Done
    posChild.do { child ->
      // if child:
      //   print to_xy(child.pos), child.wins, child.losses, child.score()
      (child == Done).ifFalse {
        ((child.wins + child.losses) > maxvisits).ifTrue {
          maxvisits := child.wins + child.losses
          maxchild := child
        }
      }
    }
    return maxchild
  }
}

// def user_move(board):
//     while True:
//         text = six.moves.input('?').strip()
//         if text == 'p':
//             return PASS
//         if text == 'q':
//             raise EOFError
//         try:
//             x, y = [int(i) for i in text.split()]
//         except ValueError:
//             continue
//         if not (0 <= x < SIZE and 0 <= y < SIZE):
//             continue
//         pos = to_pos(x, y)
//         if board.useful(pos):
//             return pos


method computerMove(board, games) {
  def pos = board.randomMove()
  // print("randomMove " + pos)
  (pos == pass).ifTrue {
    return pass
  }
  def tree = UCTNode
  tree.unexplored := board.usefulMoves()
  def nboard = Board

  0.asInteger.to(games - 1.asInteger)do { game ->
    // print("new game " + game)
    def node = tree
    nboard.reset()
    nboard.replay(board.history)
    node.play(nboard)
  }
  return tree.bestVisited().pos
}


method versusCpu(games) {
  random.setSeed(74755.asInteger)
  def board = Board
  return computerMove(board, games)
}

method asString -> String {
  "Go.grace"
}

method benchmark(innerIterations) {
  def result = versusCpu(innerIterations)
  ((innerIterations == 10)  && (result == 8)).ifTrue { return }
  ((innerIterations == 100) && (result == 1)).ifTrue { return }
  ((innerIterations == 200) && (result == 37)).ifTrue { return }

  error("Not expected or wrong result. Result is: " + result + " for " + innerIterations)
}

