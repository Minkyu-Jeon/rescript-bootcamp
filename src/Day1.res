let input =
  Node.Fs.readFileAsUtf8Sync("input/day1.sample.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.keepMap(x => x->Belt.Int.fromString)

// https://clojuredocs.org/clojure.core/reduced

let solve1 = TargetArrayMultipliedValue.find(2020, 2)
let p1 = input->solve1
p1->Js.log

let solve2 = TargetArrayMultipliedValue.find(2020, 3)
let p2 = input->solve2
p2->Js.log
