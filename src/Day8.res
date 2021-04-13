// Acc(int)
type instruction =
  | Acc({val: int})
  | Jmp({val: int})
  | Nop({val: int})

type resultType =
  | Infinite({val: int})
  | Finite({val: int})

let parseInstruction: string => option<instruction> = x => {
  switch x->Js.String2.split(" ") {
  | [inst, val] => {
    // optionIntVal: option<int>
      let optionIntVal = switch val->Js.String2.get(0) {
      | "+" => val->Js.String2.sliceToEnd(~from=1)->Belt.Int.fromString
      | "-" =>
        val->Js.String2.sliceToEnd(~from=1)->Belt.Int.fromString->Belt.Option.map(x => x * -1)
      | _ => None
      }

    // 이 두 함수를 공부하고, 리팩토링해보기
    // Belt.Option.map
    // Belt.Option.flatMap

    // instruction: option<instruction>
      let instruction = switch optionIntVal {
      | Some(intVal) =>
        switch inst {
        | "acc" => Some(Acc({val: intVal}))
        | "jmp" => Some(Jmp({val: intVal}))
        | "nop" => Some(Nop({val: intVal}))
        | _ => None
        }
      | None => None
      }
    }
  | _ => None
  }
}

// js -> parseInt: string => int | NaN
// string => array<string> => array<option<instruction>> => array<instruction>
let input =
  Node.Fs.readFileAsUtf8Sync("input/day8.sample.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.keepMap(parseInstruction)

let rec solve = (input, acc, set, index) => {
  let optionItem = input->Belt.Array.get(index)

  if set->Belt.Set.Int.has(index) {
    Infinite({val: acc})
  } else {
    switch optionItem {
    | Some(item) =>
      switch item {
      | Acc({val}) => input->solve(acc + val, set->Belt.Set.Int.add(index), index + 1)
      | Jmp({val}) => input->solve(acc, set->Belt.Set.Int.add(index), index + val)
      | Nop(_) => input->solve(acc, set->Belt.Set.Int.add(index), index + 1)
      }
    | None => Finite({val: acc})
    }
  }
}

let p1 = input->solve(0, Belt.Set.Int.empty, 0)

let p1 = switch p1 {
| Infinite({val}) => val
| Finite({val}) => 0
}

p1->Js.log

// 영속성
let immutableArray = a => {
  let newArray = a->Belt.Array.copy
  a
}

/*
a - b - c
a - b - d
*/

input
->Belt.Array.mapWithIndex((i, x) => {
  switch x {
  | Acc(_) => None
  | Jmp({val}) => {
    // 마음이 불편한 이유...
    // input->Belt.PersistentArray.set() 같은 친구가 있으면 좋겠다.
      let result = input->immutableArray->Belt.Array.set(i, Nop({val: val}))
      Some(newInput->solve(0, Belt.Set.Int.empty, 0))
    }
  | Nop({val}) => {
      let newInput = input->Belt.Array.copy
      let result = newInput->Belt.Array.set(i, Jmp({val: val}))
      Some(newInput->solve(0, Belt.Set.Int.empty, 0))
    }
  }
})
->Belt.Array.keepMap(x => x)
->Belt.Array.keep(x => {
  switch x {
  | Infinite({val}) => false
  | Finite({val}) => true
  }
})
->Js.log
