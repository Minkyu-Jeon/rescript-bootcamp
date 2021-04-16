type instruction =
  | Acc(int)
  | Jmp(int)
  | Nop(int)

type resultType =
  | Infinite(int)
  | Finite(int)

// {acc, set, index} => 상태: programState
type programState = {
  acc: int,
  visitedIndex: Belt_SetInt.t,
  index: int,
}

type programEnv = {
  input: Vector.t<instruction>,
  transformStateFn: (programState, programState => resultType, instruction) => resultType,
  terminateCriteriaFn: programState => bool,
}

let mapWithIndex = (vec, f) => {
  let (_, acc) = vec->Vector.reduce((0, Vector.make()), ((i, newVec), item) => {
    (i + 1, Vector.push(newVec, f(i, item)))
  })
  acc
}

let parseInstruction: string => option<instruction> = x => {
  switch x->Js.String2.split(" ") {
  | [inst, val] =>
    // Belt.Option.flatMap으로 리팩토링
    val
    ->Belt.Int.fromString
    ->Belt.Option.flatMap(x => {
      switch inst {
      | "acc" => Some(Acc(x))
      | "jmp" => Some(Jmp(x))
      | "nop" => Some(Nop(x))
      | _ => None
      }
    })
  | _ => None
  }
}

// js -> parseInt: string => int | NaN
// string => array<string> => array<option<instruction>> => vector<instruction>
let input =
  Node.Fs.readFileAsUtf8Sync("input/day8.sample.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.keepMap(parseInstruction)
  ->Vector.fromArray

// 상태 변환 함수
let transformState = ({acc, visitedIndex, index}, run, instruction) => {
  switch instruction {
  | Acc(val) =>
    run({acc: acc + val, visitedIndex: visitedIndex->Belt.Set.Int.add(index), index: index + 1})
  | Jmp(val) =>
    run({acc: acc, visitedIndex: visitedIndex->Belt.Set.Int.add(index), index: index + val})
  | Nop(_) => run({acc: acc, visitedIndex: visitedIndex->Belt.Set.Int.add(index), index: index + 1})
  }
}

// 종료 조건
let terminateCriteria = state => state.visitedIndex->Belt.Set.Int.has(state.index)

// 재귀
let rec run = (env: programEnv, state: programState): resultType => {
  if env.terminateCriteriaFn(state) {
    Infinite(state.acc)
  } else {
    env.input
    ->Vector.get(state.index)
    ->Belt.Option.mapWithDefault(Finite(state.acc), transformState(state, run(env)))
  }
}

// input = 환경: program
let env = {input: input, transformStateFn: transformState, terminateCriteriaFn: terminateCriteria}

let p1_1 = env->run({acc: 0, visitedIndex: Belt.Set.Int.empty, index: 0})

switch p1_1 {
| Infinite(val) => val
| Finite(_) => 0
}->Js.log

/*

let x = [1, 2, 3]

let isEven: int => bool

x->Array.map(isEven) : [false, true, false]
x->Array.keep(x => x) : [true]

x->Array.keep(isEven) : [2]
*/

// flatMap
let p2 =
  input
  ->mapWithIndex((i, x) => {
    switch x {
    | Acc(_) => None
    | Jmp(val) =>
      input
      ->Vector.set(i, Nop(val))
      ->Belt.Option.map(input => {
        let env = {
          input: input,
          transformStateFn: transformState,
          terminateCriteriaFn: terminateCriteria,
        }
        env->run({acc: 0, visitedIndex: Belt.Set.Int.empty, index: 0})
      })
    | Nop(val) =>
      input
      ->Vector.set(i, Jmp(val))
      ->Belt.Option.map(input => {
        let env = {
          input: input,
          transformStateFn: transformState,
          terminateCriteriaFn: terminateCriteria,
        }
        env->run({acc: 0, visitedIndex: Belt.Set.Int.empty, index: 0})
      })
    }
  })
  ->Vector.keepMap(Garter.Fn.identity)
  ->Vector.keep(x => {
    switch x {
    | Finite(_) => true
    | _ => false
    }
  })
  ->Vector.get(0) // (take 5 collection)

/*
// 1
input
->Belt.Array.map()
->Belt.Array.map()
->Belt.Array.keep()
->Ta

// 2
let x = input->Belt.Array.map()
let y = x->Belt.Array.map()
let z = z->Belt.Array.keep()
*/

p2
->Belt.Option.mapWithDefault(0, p2 => {
  switch p2 {
  | Infinite(_) => 0
  | Finite(val) => val
  }
})
->Js.log

let rec solve3 = (input, acc, set, index, flag) => {
  let optionItem = input->Vector.get(index)

  if set->Belt.Set.Int.has(index) {
    None
  } else {
    optionItem->Belt.Option.mapWithDefault(Some(acc), instruction => {
      switch instruction {
      | Acc(val) => input->solve3(acc + val, set->Belt.Set.Int.add(index), index + 1, flag)
      | Jmp(val) =>
        if flag {
          let result = input->solve3(acc, set->Belt.Set.Int.add(index), index + 1, false)
          switch result {
          | Some(acc) => Some(acc)
          | None => input->solve3(acc, set->Belt.Set.Int.add(index), index + val, true)
          }
        } else {
          input->solve3(acc, set->Belt.Set.Int.add(index), index + val, false)
        }
      | Nop(val) =>
        if flag {
          let result = input->solve3(acc, set->Belt.Set.Int.add(index), index + val, false)
          switch result {
          | Some(acc) => Some(acc)
          | None => input->solve3(acc, set->Belt.Set.Int.add(index), index + 1, true)
          }
        } else {
          input->solve3(acc, set->Belt.Set.Int.add(index), index + 1, false)
        }
      }
    })
  }
}

let p2BackTracking = input->solve3(0, Belt.Set.Int.empty, 0, true)
p2BackTracking->Js.log
