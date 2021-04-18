let input = Util.parseInputByNewlineMapToInt("input/2020/day9.sample.txt")

let makeWindow = (arr, ~windowSize: int, ~step: int) => {
  let len = arr->Array.length - windowSize + 1

  let (_, result) =
    arr
    ->Array.slice(~offset=0, ~len)
    ->Array.reduce((0, []), ((offset, acc), _) => {
      let chunk = [arr->Array.slice(~offset, ~len=windowSize)]
      (offset + step, acc->Array.concat(chunk))
    })

  result
}

// P1.
// 1. 슬라이딩 윈도우의 배열을 만든다
// 2. preambleLength 이후의 배열과 합친다
// 3. Day1의 모듈을 이용해서 값을 찾는다
// 4. None이 나온 부분을 찾는다
let preambleLength = 25
let windows = input->makeWindow(~windowSize=preambleLength, ~step=1)
let leftArray = input->Array.sliceToEnd(preambleLength)
let p1 =
  leftArray
  ->Array.zip(windows)
  ->Array.map(((targetNumber, window)) => {
    let result = TargetArrayMultipliedValue.find(targetNumber, 2, window)
    switch result {
    | Some(_) => None
    | None => Some(targetNumber)
    }
  })
  ->Array.keepMap(Garter.Fn.identity)
  ->Array.get(0)
p1->Js.log

// P2.
// 1. 1번 값보다 작은 값들만 남긴다
// 재귀적으로 아래 실행 사이즈 2부터 증가하면서
// 2. 슬라이딩 윈도우를 만든다
// 3. 합을 찾는다
// 4. 찾으면 값 반환 아니면 재귀

type env = {
  targetValue: int,
  input: array<int>,
  terminateCriteriaFn: (int, int) => bool,
}

let rec p2Solve = (env, windowSize) => {
  let windows = env.input->makeWindow(~windowSize, ~step=1)->List.fromArray

  let rec recursion = windows => {
    switch windows {
    | list{} => p2Solve(env, windowSize + 1)
    | list{head, ...rest} =>
      if head->Garter.Math.sum_int->env.terminateCriteriaFn(env.targetValue) {
        head
      } else {
        recursion(rest)
      }
    }
  }

  recursion(windows)
}

let p2 = p1->Option.map(p1 => {
  let newInput = input->Array.keep(x => x < p1)
  let terminateCriteria = (sumOfArr, targetValue) => sumOfArr == targetValue
  let env = {
    targetValue: p1,
    input: newInput,
    terminateCriteriaFn: terminateCriteria,
  }
  let result = env->p2Solve(2)->Set.Int.fromArray

  result->Set.Int.minimum->Option.getWithDefault(0) +
    result->Set.Int.maximum->Option.getWithDefault(0)
})

p2->Js.log
