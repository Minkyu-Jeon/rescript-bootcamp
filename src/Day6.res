let input =
  Node.Fs.readFileAsUtf8Sync("input/day6.sample.txt")
  ->Js.String2.split("\n\n")
  ->Belt.Array.map(x =>
    x
    ->Js.String2.split("\n")
    ->Belt.Array.map(x => x->Js.String2.split("")->Belt.Set.String.fromArray)
  )

let unionSet = x => {
  x->Belt.Array.reduce(Belt.Set.String.empty, (acc, item) => acc->Belt.Set.String.union(item))
}

let intersectSet = x => {
  x->Garter.Array.reduce1U((. acc, item) => item->Belt.Set.String.intersect(acc))
}

let solve = (input, aggrigator) => {
  input
  ->Belt.Array.map(aggrigator)
  ->Belt.Array.map(Belt.Set.String.size)
  ->Belt.Array.reduce(0, (acc, item) => acc + item)
}

let p1 = input->solve(unionSet)

p1->Js.log

let p2 = input->solve(intersectSet)

p2->Js.log
