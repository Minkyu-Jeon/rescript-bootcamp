let input = Util.parseInputByNewlineMapToInt("input/day9.sample.txt")

let rec solve = (input, preambleLength) => {
  if input->Array.length < preambleLength + 1 {
    None
  } else {
    let result =
      input
      ->Array.get(preambleLength)
      ->Option.flatMap(x => {
        let solveFn = Day1.solve(x, 2)
        let data = input->Array.slice(~offset=0, ~len=preambleLength)->solveFn
        switch data {
        | Some(_) => None
        | None => Some(x)
        }
      })

    switch result {
    | Some(value) => Some(value)
    | None => input->Array.sliceToEnd(1)->solve(preambleLength)
    }
  }
}

let p1 = input->solve(25)
p1->Js.log

let rec calc = (input, targetLength, targetSum) => {
  if input->Array.length <= targetLength {
    None
  } else {
    let subarray = input->Array.slice(~offset=0, ~len=targetLength)
    if subarray->Garter.Math.sum_int == targetSum {
      Some(subarray)
    } else {
      input->Array.sliceToEnd(1)->calc(targetLength, targetSum)
    }
  }
}

let rec solve2 = (input, targetLength, targetSum) => {
  if input->Array.length < targetLength {
    None
  } else {
    switch input->calc(targetLength, targetSum) {
    | Some(value) => Some(value)
    | None => input->solve2(targetLength + 1, targetSum)
    }
  }
}

let p2 = p1
->Option.flatMap(p1 => input->Array.keep(x => x < p1)->solve2(2, p1))
->Option.map(Belt.SortArray.Int.stableSort)
->Option.map(x => {
  let arrLen = x->Array.length
  let first = x->Array.getExn(0)
  let last = x->Array.getExn(arrLen - 1)

  first + last
})

p2->Js.log
