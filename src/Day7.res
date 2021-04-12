type bagWithQuantity = {
  quantity: int,
  bagName: string,
}

type bag = {
  bagName: string,
  includingBags: array<bagWithQuantity>,
}

let parseIncludedBags = x => {
  x
  ->Js.String2.replace(".", "")
  ->Js.String2.split(", ")
  ->Belt.Array.keepMap(x => {
    switch x->Js.String2.split(" ") {
    | [quantity, pattern, color, _] =>
      Some({
        quantity: quantity->Belt.Int.fromString->Belt.Option.getWithDefault(0),
        bagName: `${pattern} ${color}`,
      })
    | _ => None
    }
  })
}

let parseLine = (x: string): option<bag> => {
  switch x->Js.String2.split(" contain ") {
  | [containerBags, includedBags] =>
    let containerBags = switch containerBags->Js.String2.split(" ") {
    | [pattern, color, _] => `${pattern} ${color}`
    | _ => ""
    }
    Some({bagName: containerBags, includingBags: parseIncludedBags(includedBags)})
  | _ => None
  }
}

let input =
  Node.Fs.readFileAsUtf8Sync("input/day7.sample.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.keepMap(parseLine)

let rec find = (input, bagName, set) => {
  let parents = input->Belt.Array.reduce([], (acc, item) => {
    if item.includingBags->Belt.Array.some(x => x.bagName == bagName) {
      acc->Belt.Array.concat([item.bagName])
    } else {
      acc
    }
  })

  switch parents {
  | [] => set
  | _ =>
    parents->Belt.Array.reduce(set, (acc, item) => {
      if set->Belt.Set.String.has(item) {
        acc
      } else {
        input->find(item, acc->Belt.Set.String.add(item))
      }
    })
  }
}

let p1 = input->find("shiny gold", Belt.Set.String.empty)->Belt.Set.String.size

p1->Js.log
