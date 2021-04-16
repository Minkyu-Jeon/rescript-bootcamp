type instruction =
  | Acc(int)
  | Jmp(int)
  | Nop(int)

type resultType =
  | Infinite(int)
  | Finite(int)

// {acc, set, index} => 상태: programState
type programState =
  | ProgramState1({acc: int, visitedIndex: Belt_SetInt.t, index: int})
  | ProgramState2({acc: int, visitedIndex: Belt_SetInt.t, index: int, flag: bool})

let getAcc = (state: programState) => {
  switch state {
  | ProgramState1({acc, _}) => acc
  | ProgramState2({acc, _}) => acc
  }
}

let getIndex = (state: programState) => {
  switch state {
  | ProgramState1({index, _}) => index
  | ProgramState2({index, _}) => index
  }
}

let getvisitedIndex = (state: programState) => {
  switch state {
  | ProgramState1({visitedIndex, _}) => visitedIndex
  | ProgramState2({visitedIndex, _}) => visitedIndex
  }
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
let transformState = (state, run, instruction) => {
  switch state {
  | ProgramState1({acc, visitedIndex, index}) =>
    switch instruction {
    | Acc(val) =>
      run(
        ProgramState1({
          acc: acc + val,
          visitedIndex: visitedIndex->Belt.Set.Int.add(index),
          index: index + 1,
        }),
      )
    | Jmp(val) =>
      run(
        ProgramState1({
          acc: acc,
          visitedIndex: visitedIndex->Belt.Set.Int.add(index),
          index: index + val,
        }),
      )
    | Nop(_) =>
      run(
        ProgramState1({
          acc: acc,
          visitedIndex: visitedIndex->Belt.Set.Int.add(index),
          index: index + 1,
        }),
      )
    }
  | ProgramState2({acc, visitedIndex, index, flag}) =>
    switch instruction {
    | Acc(val) =>
      run(
        ProgramState2({
          acc: acc + val,
          visitedIndex: visitedIndex->Belt.Set.Int.add(index),
          index: index + 1,
          flag: flag,
        }),
      )
    | Jmp(val) =>
      if flag {
        let result = run(
          ProgramState2({
            acc: acc,
            visitedIndex: visitedIndex->Belt.Set.Int.add(index),
            index: index + 1,
            flag: false,
          }),
        )
        switch result {
        | Finite(acc) => Finite(acc)
        | Infinite(_) =>
          run(
            ProgramState2({
              acc: acc,
              visitedIndex: visitedIndex->Belt.Set.Int.add(index),
              index: index + val,
              flag: true,
            }),
          )
        }
      } else {
        run(
          ProgramState2({
            acc: acc,
            visitedIndex: visitedIndex->Belt.Set.Int.add(index),
            index: index + val,
            flag: false,
          }),
        )
      }
    | Nop(val) =>
      if flag {
        let result = run(
          ProgramState2({
            acc: acc,
            visitedIndex: visitedIndex->Belt.Set.Int.add(index),
            index: index + val,
            flag: false,
          }),
        )
        switch result {
        | Finite(acc) => Finite(acc)
        | Infinite(_) =>
          run(
            ProgramState2({
              acc: acc,
              visitedIndex: visitedIndex->Belt.Set.Int.add(index),
              index: index + 1,
              flag: true,
            }),
          )
        }
      } else {
        run(
          ProgramState2({
            acc: acc,
            visitedIndex: visitedIndex->Belt.Set.Int.add(index),
            index: index + 1,
            flag: false,
          }),
        )
      }
    }
  }
}

// 종료 조건
let terminateCriteria = (state: programState) => {
  state->getvisitedIndex->Belt.Set.Int.has(state->getIndex)
}

// 재귀
let rec run = (env: programEnv, state: programState): resultType => {
  if env.terminateCriteriaFn(state) {
    Infinite(state->getAcc)
  } else {
    env.input
    ->Vector.get(state->getIndex)
    ->Belt.Option.mapWithDefault(Finite(state->getAcc), transformState(state, run(env)))
  }
}

// input = 환경: program
let env = {input: input, transformStateFn: transformState, terminateCriteriaFn: terminateCriteria}

let p1_1 = env->run(ProgramState1({acc: 0, visitedIndex: Belt.Set.Int.empty, index: 0}))

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
        env->run(ProgramState1({acc: 0, visitedIndex: Belt.Set.Int.empty, index: 0}))
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
        env->run(ProgramState1({acc: 0, visitedIndex: Belt.Set.Int.empty, index: 0}))
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
p2
->Belt.Option.mapWithDefault(0, p2 => {
  switch p2 {
  | Infinite(_) => 0
  | Finite(val) => val
  }
})
->Js.log

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

let p2BackTracking =
  env->run(ProgramState2({acc: 0, visitedIndex: Belt.Set.Int.empty, index: 0, flag: true}))

switch p2BackTracking {
| Infinite(_) => 0
| Finite(val) => val
}->Js.log


// 환경 주입시 별도의 함수를 주입하는게 나을듯..
// run에서 조금만 수정하면