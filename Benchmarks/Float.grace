def points: Number = 100000

class Point(i) {
  var x := i.sin
  var y := i.cos * 3
  var z := x * x / 2

  method normalize {
    var x' := self.x
    var y' := self.y
    var z' := self.z
    var norm := (x * x + y * y + z * z).sqrt
    self.x := self.x / norm
    self.y := self.y / norm
    self.z := self.z / norm
  }

  method maximize(other) {
    x := (x > other.x).ifTrue {x} ifFalse {other.x}
    y := (y > other.y).ifTrue {y} ifFalse {other.y}
    z := (z > other.z).ifTrue {z} ifFalse {other.z}
    return self
  }

  method asString {
    "<Point: x=" + x + " y=" + y + " z=" + z + ">"
  }
}

method maximize(points) {
  var next := points.at(1.asInteger)
  2.asInteger.to(points.size) do { i ->
    def p = points.at(i)
    next := next.maximize(p)
  }
  return next
}

method asString -> String {
  "Float.grace"
}

method benchmark(innerIterations) {
  var points := platform.kernel.Array.new(innerIterations)

  1.asInteger.to(innerIterations) do { i ->
    points.at(i)put(Point(i - 1))
  }

  points.do { p ->
    p.normalize
  }

  def point = maximize(points)
  ((point.x == 0.8943770142484665) &&
   (point.y == 1) &&
   (point.z == 0.4471849230147518)).ifFalse {
    error("Result looks incorrect.")
  }
}
