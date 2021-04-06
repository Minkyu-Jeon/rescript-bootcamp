let input = Node.Fs.readFileAsUtf8Sync("input/day3.sample.txt")->Js.String2.split("\n")->Belt.Array.map(x => x->Js.String2.split(""))

let rec countTree = (totalLine, index, treeCount, moveRight) => {
  switch totalLine {
  | [[]] => treeCount
  | _ => {
      let line = totalLine->Belt.Array.get(0)
      switch line {
      | None => treeCount
      | Some(value) => {
          let lineLength = value->Belt.Array.length
          let rest = totalLine->Belt.Array.sliceToEnd(1)
          let nextIndex = index + moveRight
          if value->Belt.Array.get(index) == Some("#") {
            countTree(rest, mod(nextIndex, lineLength), treeCount + 1, moveRight)
          } else {
            countTree(rest, mod(nextIndex, lineLength), treeCount, moveRight)
          }
        }
      }
    }
  }
}

let rec multiplyTraverses = (traverses, input, reducer, accumulator: float) => {
    switch traverses {
        | list{} => accumulator
        | list{trav, ...rest} => {
            let (right, down) = trav
            let newInput = input->Belt.Array.keepWithIndex((_x, i) => mod(i, down) == 0)
            let treeCount = newInput->reducer(0, 0, right)->Belt.Int.toFloat
            let acc = treeCount *. accumulator
            multiplyTraverses(rest, input, reducer, acc)
        }
    }
}

let solve = (traverses, func, input, reducer) => {
    func(traverses, input, reducer, 1.0)
}


// {right: 1, down: 1}, {right: 3, down: 1}, {right: 5, down: 1}, {right: 7, down: 1}, {right: 1, down: 2}
let traverses = list{(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)}

// Js.String2 : data-first vs Js.String : data-last

traverses->solve(multiplyTraverses, input, countTree)->Js.log
