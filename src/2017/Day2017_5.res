let input = Util.parseInputByNewlineMapToInt("input/2017/day5.sample.txt")->Vector.fromArray

type programState = {index: int, count: int, acc: Belt_MapInt.t<int>}

let nextState = (stepFn, {index, count, acc}, x) => {
  let accValue = acc->Map.Int.getWithDefault(index, 0)
  let step = stepFn(x, accValue)

  {index: index + x + accValue, count: count + 1, acc: acc->Map.Int.set(index, accValue + step)}
}

let totalSum = map => map->Map.Int.reduce(0, (acc, _, v) => acc + v)

let rec solve = (state, input, nextStateFn) => {
  switch input->Vector.get(state.index) {
  | None => state.count
  | Some(x) => nextStateFn(state, x)->solve(input, nextStateFn)
  }
}

let initialState = {index: 0, count: 0, acc: Map.Int.empty}

let stepFn1 = (_, _) => 1
let p1 = initialState->solve(input, nextState(stepFn1))
p1->Js.log

let stepFn2 = (x, accValue) => x + accValue >= 3 ? -1 : 1
let p2 = initialState->solve(input, nextState(stepFn2))
p2->Js.log
