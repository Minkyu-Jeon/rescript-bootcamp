type rec tree = {bagName: string, quantity: int, parents: array<tree>, children: array<tree>}

let rec makeParentTree = (input, bagName) => {
  let names =
    input->Belt.Array.keep(x =>
      x.children
      ->Belt.Array.map(x => x.bagName)
      ->Belt.Array.keep(x => x == bagName)
      ->Belt.Array.length == 1
    )

  if names->Belt.Array.length == 0 {
    []
  } else {
    names->Belt.Array.map(x => {
      let quantity =
        x.children
        ->Belt.Array.keep(x => x.bagName == bagName)
        ->Belt.Array.get(0)
        ->Belt.Option.mapWithDefault(0, x => x.quantity)
      {
        bagName: x.bagName,
        quantity: quantity,
        children: [],
        parents: input->makeParentTree(x.bagName),
      }
    })
  }
}

let rec makeChildTree = (input, bagName) => {
  switch input->Belt.Array.keep(x => x.bagName == bagName)->Belt.Array.get(0) {
  | Some(targetNode) =>
    targetNode.children->Belt.Array.map(x => {
      {
        bagName: x.bagName,
        quantity: x.quantity,
        children: input->makeChildTree(x.bagName),
        parents: [],
      }
    })
  | None => []
  }
}

let parseTree = (str): option<tree> => {
  switch str->Js.String2.split(" ") {
  | [quantity, pattern, color] =>
    Some({
      bagName: `${pattern} ${color}`,
      quantity: quantity->Belt.Int.fromString->Belt.Option.getWithDefault(0),
      children: [],
      parents: [],
    })
  | [pattern, color] =>
    let bagName = `${pattern} ${color}`
    if bagName == "no other" {
      None
    } else {
      Some({bagName: bagName, quantity: 1, parents: [], children: []})
    }
  | _ => None
  }
}

let parseSubTree = (str): array<tree> => {
  str->Js.String2.split(", ")->Belt.Array.keepMap(parseTree)
}

let preprocess = str => {
  str
  ->Js.String2.replaceByRe(%re("/ bags/g"), "")
  ->Js.String2.replaceByRe(%re("/ bag/g"), "")
  ->Js.String2.replaceByRe(%re("/\./g"), "")
  ->Js.String2.split("\n")
  ->Belt.Array.keepMap(x => {
    switch x->Js.String2.split(" contain ") {
    | [container, includedBags] =>
      switch parseTree(container) {
      | Some(containerNode) => Some({...containerNode, children: parseSubTree(includedBags)})
      | None => None
      }
    | _ => None
    }
  })
}

let input = Node.Fs.readFileAsUtf8Sync("input/day7.sample.txt")->preprocess

let p1 = input
->makeParentTree("shiny gold")
->Belt.Array.reduce(Belt.Set.String.empty, (acc, item) => {
  let rec recursive = (acc, item) => {
    if item.parents->Belt.Array.length == 0 {
      acc->Belt.Set.String.add(item.bagName)
    } else {
      item.parents->Belt.Array.reduce(acc->Belt.Set.String.add(item.bagName), recursive)
    }
  }
  acc->recursive(item)
})
->Belt.Set.String.size

p1->Js.log

let p2 =
  input
  ->makeChildTree("shiny gold")
  ->Belt.Array.reduce(0, (acc, item) => {
    let rec recursive = item => {
      if item.children->Belt.Array.length == 0 {
        item.quantity
      } else {
        item.children->Belt.Array.reduce(item.quantity, (acc, item2) => {
          acc + item.quantity * recursive(item2)
        })
      }
    }
    acc + recursive(item)
  })

p2->Js.log
