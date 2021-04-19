let input = Util.parseInputByNewlineMapToInt("input/2017/day5.sample.txt")->Vector.fromArray

type programState = {index: int, acc: Belt_MapInt.t<int>}

let nextState = ({index, acc}, x) => {
  let count = acc->Map.Int.getWithDefault(index, 0)

  {index: index + x + count, acc: acc->Map.Int.set(index, count + 1)}
}

let totalSum = map => map->Map.Int.reduce(0, (acc, _, v) => acc + v)

let rec solve = (state, input) => {
  switch input->Vector.get(state.index) {
    | None => state.acc->totalSum
    | Some(x) => nextState(state, x)->solve(input)
  }
}

{index: 0, acc: Map.Int.empty}->solve(input)->Js.log