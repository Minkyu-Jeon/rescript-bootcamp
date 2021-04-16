let rec calc = (targetArr, input, targetSum, targetLength) => {
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
          ->calc(input->Belt.Array.sliceToEnd(1), targetSum, targetLength)
        })

      switch result {
      | Some(val) => Some(val)
      | None =>
        calc(
          targetArr,
          input->Belt.Array.sliceToEnd(1),
          targetSum,
          targetLength,
        )
      }
    }
  } else if targetArr->Garter.Math.sum_int == targetSum {
    Some(targetArr->Util.multiply_int)
  } else {
    None
  }
}

let rec find = (targetSum, targetLength, input) => {
  if input->Belt.Array.length < targetLength {
    None
  } else {
    let result =
      input
      ->Belt.Array.get(0)
      ->Belt.Option.flatMap(x => {
        [x]->calc(
          input->Belt.Array.sliceToEnd(1),
          targetSum,
          targetLength,
        )
      })
    switch result {
    | Some(val) => Some(val)
    | None => find(targetSum, targetLength, input->Belt.Array.sliceToEnd(1))
    }
  }
}