// The Computer Language Benchmarks Game
//  https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
//  contributed by Isaac Gouy 
//
// <reference path="./Include/node/index.d.ts" />
//
//
// Translated to Grace by Richard Roberts, 28/05/2018
//

method approximate(n: Number) -> Number {
   def u: List = platform.kernel.Vector.new
   def v: List = platform.kernel.Vector.new

   1.asInteger.to(n) do { i -> u.append(1) }
   1.asInteger.to(n) do { i -> v.append(0) }

   1.asInteger.to(10.asInteger) do { i ->
      multiplyAtAv(n,u,v)
      multiplyAtAv(n,v,u)
   }

   var vBv: Number := 0
   var  vv: Number := 0
   1.asInteger.to(10.asInteger) do { i ->
      vBv := vBv + u.at(i) * v.at(i)
      vv  := vv  + v.at(i) * v.at(i)
   }

   (vBv / vv).sqrt
}

method a(i: Number, j: Number) -> Number {
   1 / ( (i + j) * ((i + j) + 1) / 2 + i + 1 ) 
}

method multiplyAv(n: Number, v: List, av: List) -> Done {
   0.asInteger.to(n - 2.asInteger) do { i ->
      av. at (i + 1.asInteger) put (0)
      0.asInteger.to(n - 2.asInteger) do { j ->
         av.at(i + 1.asInteger) put ( av.at(i + 1.asInteger) + a(i, j) * v.at(j + 1.asInteger) )
      }
   }

   Done
}

method multiplyAtv(n: Number, v: List, atv: List) -> Done {
   0.asInteger.to(n - 2.asInteger) do { i ->
      atv. at (i + 1.asInteger) put (0)
      0.asInteger.to(n - 2.asInteger) do { j ->
         atv. at (i + 1.asInteger) put ( atv.at(i + 1.asInteger) + a(j, i) * v.at(j + 1.asInteger) )
      }
   }

   Done
}

method multiplyAtAv(n: Number, v: List, atAv: List) -> Done {
   def u = platform.kernel.Vector.new
   1.asInteger.to(n) do { i -> u.append(0) }
   multiplyAv(n,v,u)
   multiplyAtv(n,u,atAv)
   Done
}


method asString -> String {
  "SpectralNorm.grace"
}


method benchmark(innerIterations) {
   1.asInteger.to(innerIterations) do { i ->
      var result: Number := approximate(5500)
      print(result)
      var difference: Number := (result - 1.2742241527924973).abs
      if (difference < 0.00001) then {
         error("{self} failed, {result} !~= 1.2742241527924973")
      }
   }
}
