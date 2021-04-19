let input = Util.parseInputByNewlineMapToInt("input/2017/day5.sample.txt")->Vector.fromArray

type programState = {index: int, count: int, acc: Belt_MapInt.t<int>}

let nextState = ({index, count, acc}, x) => {
  let accValue = acc->Map.Int.getWithDefault(index, 0)

  {index: index + x + accValue, count: count + 1, acc: acc->Map.Int.set(index, accValue + 1)}
}

let nextState2 = ({index, count, acc}, x) => {
  let accValue = acc->Map.Int.getWithDefault(index, 0)
  let step = (x + accValue) >= 3 ? -1 : 1

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

let p1 = initialState->solve(input, nextState)
p1->Js.log

let p2 = initialState->solve(input, nextState2)
p2->Js.log