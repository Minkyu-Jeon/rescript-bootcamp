let input =
  Node.Fs.readFileAsUtf8Sync("input/day6.sample.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(x => x->Js.String2.split(""))

input
->Belt.Array.reduce(list{list{[]}}, (acc, line) => {
  if line->Belt.Array.length == 0 {
    acc->Belt.List.add(list{})
  } else {
    let head = acc->Belt.List.head->Belt.Option.getWithDefault(list{})
    let newAcc = acc->Belt.List.drop(1)->Belt.Option.getWithDefault(list{list{}})
    newAcc->Belt.List.add(head->Belt.List.add(line))
  }
})
->Belt.List.map(x => {
  x->Belt.List.map((line) => {
    line->Belt.Set.String.fromArray
  })->Belt.List.reduceWithIndex(Belt.Set.String.empty, (acc, item, i) => {
      if ( i == 0 ) {
          item
      } else {
          acc->Belt.Set.String.intersect(item)
      }
  })
})
->Belt.List.reduce(0, (acc, x) => acc + x->Belt.Set.String.size)
->Js.log
// ->Belt.List.reduce(0, (acc, x) => acc + x->Belt.Set.String.size)
// ->Js.log
