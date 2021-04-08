let input =
  Node.Fs.readFileAsUtf8Sync("input/day6.sample.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(x => x->Js.String2.split(""))

let groupAnswers = input => {
  input->Belt.Array.reduce(list{list{}}, (acc, line) => {
    if line->Belt.Array.length == 0 {
      acc->Belt.List.add(list{})
    } else {
      switch acc {
      | list{} => acc
      | list{head} => list{head->Belt.List.add(line)}
      | list{head, ...rest} => rest->Belt.List.add(head->Belt.List.add(line))
      }
    }
  })
}

let unionSet = x => {
  x->Belt.List.reduce(Belt.Set.String.empty, (acc, item) => acc->Belt.Set.String.union(item))
}

let insersectSet = x => {
  switch x {
  | list{} => Belt.Set.String.empty
  | list{head} => head
  | list{head, ...rest} =>
    rest->Belt.List.reduce(head, (acc, item) => acc->Belt.Set.String.intersect(item))
  }
}

let solve = (input, aggrigator) => {
  input
  ->groupAnswers
  ->Belt.List.map(x => x->Belt.List.map(y => y->Belt.Set.String.fromArray))
  ->Belt.List.map(aggrigator)
  ->Belt.List.map(x => x->Belt.Set.String.size)
  ->Belt.List.reduce(0, (acc, item) => acc + item)
}

let p1 = input->solve(unionSet)

p1->Js.log

let p2 = input->solve(insersectSet)

p2->Js.log
