// Copyright (c) 2001-2018 see AUTHORS file
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the 'Software'), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
//
// Adapted for Grace by Richard Roberts
//   2018, June
//

type FannkuchBenchmark = interface {
  size
  perm
  timesRotated
  atEnd
  pfannkuchen(anArray)
  makeNext
  maxPfannkuchen
  next
}

class FannkuchBenchmark {
  def size: Number = 7.asInteger
  var perm: List := 1.asInteger.to(size)
  var timesRotated: List := platform.kernel.Array.new(size)withAll(0.asInteger)
  var atEnd: Boolean := false

  method pfannkuchen (anArray: List) -> Number {
    var k: Number := 0.asInteger
    var complement: Number
    var first: Number := anArray.at(1.asInteger)

    { first == 1.asInteger }.whileFalse {
      
      k := k + 1.asInteger
      complement := first + 1.asInteger

      1.asInteger.to(first / 2.asInteger) do { i ->
        var a: Number := anArray.at(i)
        var b: Number := anArray.at(complement - i)
        anArray.at (i) put (b)
        anArray.at (complement - i) put (a)

        first := anArray.at(1.asInteger)
      }
    }

    k
  }

  method makeNext -> Done {
    
    // Generate the next permutation.
    2.asInteger.to (perm.size) do { r ->
      
      // Rotate the first r items to the left.
      var temp: Number := perm.at (1.asInteger)
      1.asInteger.to(r - 1.asInteger) do { i ->
        perm.at(i) put (perm.at(i + 1.asInteger))
      }
      perm.at (r) put (temp)

      timesRotated.at (r) put ((timesRotated.at(r) + 1.asInteger) % r)
      var remainder: Number := timesRotated.at (r)
      (remainder == 0.asInteger).ifFalse {
        return self
      }

      // After r rotations, the first r items are in their original positions.
      //   Go on rotating the first r+1 items.
    }

    // We are past the final permutation.
    atEnd := true
    Done
  }

  method maxPfannkuchen -> Number {
    var max: Number := 0.asInteger
    { atEnd }.whileFalse {
      max := max.max (pfannkuchen (next))
    }
    max
  }

  method next -> List {
    var result := perm.copy
    makeNext
    result
  }
}

method asString -> String {
  "Fannkuch.grace"
}

method benchmark(innerIterations: Number) {
  1.asInteger.to(innerIterations) do { i ->
    var instance: FannkuchBenchmark := FannkuchBenchmark
    var result: Number := instance.maxPfannkuchen
    (result == 16).ifFalse {
      error("{self} failed, {result} != 16")
    }
  }
}
