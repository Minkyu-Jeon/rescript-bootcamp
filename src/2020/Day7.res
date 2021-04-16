type rec tree = {bagName: string, quantity: int, children: array<tree>}

let parsedChildren = (str): option<(string, int)> => {
  switch str->Js.String2.split(" ") {
  | [quantity, pattern, color] =>
    Some((`${pattern} ${color}`, quantity->Belt.Int.fromString->Belt.Option.getWithDefault(0)))
  | _ => None
  }
}

let preprocess = str => {
  str
  ->Js.String2.replaceByRe(%re("/ bags/g"), "")
  ->Js.String2.replaceByRe(%re("/ bag/g"), "")
  ->Js.String2.replaceByRe(%re("/\./g"), "")
  ->Js.String2.split("\n")
  ->Belt.Array.keepMap(x => {
    switch x->Js.String2.split(" contain ") {
    | [container, includedBags] => {
        let parsedChildren =
          includedBags
          ->Js.String2.split(", ")
          ->Belt.Array.keepMap(parsedChildren)
          ->Belt.Map.String.fromArray
        Some((container, parsedChildren))
      }
    | _ => None
    }
  })
  ->Belt.Map.String.fromArray
}

let input = Node.Fs.readFileAsUtf8Sync("input/2020/day7.sample.txt")->preprocess

let rec solve1 = (input, targetBagName, acc) => {
  input->Belt.Map.String.reduce(acc, (acc, k, v) => {
    if v->Belt.Map.String.has(targetBagName) {
      input->solve1(k, acc->Belt.Set.String.add(k))
    } else {
      acc
    }
  })
}

let rec solve2 = (input, targetBagName, quantity) => {
  input
  ->Belt.Map.String.get(targetBagName)
  ->Belt.Option.map(x => {
    x->Belt.Map.String.reduce(quantity, (acc, k, v) => {
      acc + quantity * input->solve2(k, v)
    })
  })
  ->Belt.Option.getWithDefault(quantity)
}

let p1 = input->solve1("shiny gold", Belt.Set.String.empty)->Belt.Set.String.size

p1->Js.log

let rootContainerBagCount = 1
let p2 = input->solve2("shiny gold", rootContainerBagCount) - rootContainerBagCount

p2->Js.log
