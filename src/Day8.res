type instruction =
  | Acc(int)
  | Jmp(int)
  | Nop(int)

type resultType =
  | Infinite(int)
  | Finite(int)

// {acc, set, index} => 상태: programState
type programState = {acc: int, visitedIndex: Belt_SetInt.t, index: int, flag: bool}

type instructionFn = {
  acc: (programState, int) => programState,
  jmp: (programState, int) => programState,
  nop: programState => programState,
}

type programEnv = {
  input: Vector.t<instruction>,
  transformStateFn: (programState, instructionFn, instruction) => programState,
  terminateCriteriaFn: programState => bool,
  instructionFn: instructionFn,
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

// 각 명령어의 실제 동작
let acc = ({acc, visitedIndex, index, flag}, val) => {
  {
    acc: acc + val,
    visitedIndex: visitedIndex->Belt.Set.Int.add(index),
    index: index + 1,
    flag: flag,
  }
}
let jmp = ({acc, visitedIndex, index, flag}, val) => {
  {acc: acc, visitedIndex: visitedIndex->Belt.Set.Int.add(index), index: index + val, flag: flag}
}
let nop = ({acc, visitedIndex, index, flag}) => {
  {acc: acc, visitedIndex: visitedIndex->Belt.Set.Int.add(index), index: index + 1, flag: flag}
}

// 상태 변환 함수
// (programState, instructionFn, instruction) => programState
let transformState = (state, instructionFn, instruction) => {
  switch instruction {
  | Acc(val) => state->instructionFn.acc(val)
  | Jmp(val) =>
    state.flag
      ? {...state, flag: false}->instructionFn.nop
      : {...state, flag: false}->instructionFn.jmp(val)
  | Nop(val) =>
    state.flag
      ? {...state, flag: false}->instructionFn.jmp(val)
      : {...state, flag: false}->instructionFn.nop
  }
}

// 종료 조건
let terminateCriteria = (state: programState) => state.visitedIndex->Belt.Set.Int.has(state.index)

// 재귀
let rec run = (env: programEnv, state: programState): resultType => {
  if env.terminateCriteriaFn(state) {
    Infinite(state.acc)
  } else {
    env.input
    ->Vector.get(state.index)
    ->Belt.Option.mapWithDefault(Finite(state.acc), instruction => {
      let nextState = transformState(state, env.instructionFn, instruction)
      if state.flag {
        switch run(env, nextState) {
        | Finite(acc) => Finite(acc)
        | Infinite(_) => {
            let nextState = transformState({...state, flag: false}, env.instructionFn, instruction)
            let nextState = {...nextState, flag: true}
            run(env, nextState)
          }
        }
      } else {
        run(env, nextState)
      }
    })
  }
}

let getResultValue = (result: resultType) => {
  switch result {
  | Infinite(acc) => acc
  | Finite(acc) => acc
  }
}

// input = 환경: program
let env = {
  input: input,
  transformStateFn: transformState,
  terminateCriteriaFn: terminateCriteria,
  instructionFn: {acc: acc, jmp: jmp, nop: nop},
}

let p1 = env->run({acc: 0, visitedIndex: Belt.Set.Int.empty, index: 0, flag: false})
p1->getResultValue->Js.log

let p2BackTracking = env->run({acc: 0, visitedIndex: Belt.Set.Int.empty, index: 0, flag: true})
p2BackTracking->getResultValue->Js.log

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
          instructionFn: {acc: acc, jmp: jmp, nop: nop},
        }
        env->run({acc: 0, visitedIndex: Belt.Set.Int.empty, index: 0, flag: false})
      })
    | Nop(val) =>
      input
      ->Vector.set(i, Jmp(val))
      ->Belt.Option.map(input => {
        let env = {
          input: input,
          transformStateFn: transformState,
          terminateCriteriaFn: terminateCriteria,
          instructionFn: {acc: acc, jmp: jmp, nop: nop},
        }
        env->run({acc: 0, visitedIndex: Belt.Set.Int.empty, index: 0, flag: false})
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
  ->Vector.get(0)
// (take 5 collection)
p2->Belt.Option.mapWithDefault(0, getResultValue)->Js.log

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
