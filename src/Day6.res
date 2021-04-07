let input =
  Node.Fs.readFileAsUtf8Sync("input/day6.sample.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(x => x->Js.String2.split(""))

input
->Belt.Array.reduce(list{Belt.Set.String.empty}, (acc, line) => {
  if line->Belt.Array.length == 0 {
    acc->Belt.List.add(Belt.Set.String.empty)
  } else {
    let head = acc->Belt.List.head->Belt.Option.getWithDefault(Belt.Set.String.empty)
    let newAcc = acc->Belt.List.drop(1)->Belt.Option.getWithDefault(list{Belt.Set.String.empty})
    newAcc->Belt.List.add(line->Belt.Array.reduce(head, Belt.Set.String.add))
  }
})
->Belt.List.reduce(0, (acc, x) => acc + x->Belt.Set.String.size)
->Js.log
