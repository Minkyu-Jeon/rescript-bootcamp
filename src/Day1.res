let input =
  Node.Fs.readFileAsUtf8Sync("input/day1.sample.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.keepMap(x => x->Belt.Int.fromString)

let rec findTargetArrayMultipliedValue = (targetArr, input, targetSum, targetLength) => {
  if targetArr->Belt.Array.length < targetLength {
    if input->Belt.Array.length == 0 {
      None
    } else {
      let result =
        input
        ->Belt.Array.get(0)
        ->Belt.Option.flatMap(x => {
          targetArr
          ->Belt.Array.concat([x])
          ->findTargetArrayMultipliedValue(input->Belt.Array.sliceToEnd(1), targetSum, targetLength)
        })

      switch result {
      | Some(val) => Some(val)
      | None =>
        findTargetArrayMultipliedValue(
          targetArr,
          input->Belt.Array.sliceToEnd(1),
          targetSum,
          targetLength,
        )
      }
    }
  } else if targetArr->Belt.Array.reduce(0, \"+") == targetSum {
    Some(targetArr->Belt.Array.reduce(1, \"*"))
  } else {
    None
  }
}

let rec solve = (targetSum, targetLength, input) => {
  if input->Belt.Array.length < targetLength {
    None
  } else {
    let result =
      input
      ->Belt.Array.get(0)
      ->Belt.Option.flatMap(x => {
        [x]->findTargetArrayMultipliedValue(
          input->Belt.Array.sliceToEnd(1),
          targetSum,
          targetLength,
        )
      })
    switch result {
    | Some(val) => Some(val)
    | None => solve(targetSum, targetLength, input->Belt.Array.sliceToEnd(1))
    }
  }
}

let solve1 = solve(2020, 2)
let p1 = input->solve1
p1->Js.log

let solve2 = solve(2020, 3)
let p2 = input->solve2
p2->Js.log
