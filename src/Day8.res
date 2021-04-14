type instruction =
  | Acc(int)
  | Jmp(int)
  | Nop(int)

type resultType =
  | Infinite(int)
  | Finite(int)

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

let rec solve = (input, acc, set, index) => {
  let optionItem = input->Vector.get(index)

  if set->Belt.Set.Int.has(index) {
    Infinite(acc)
  } else {
    optionItem->Belt.Option.mapWithDefault(Finite(acc), item => {
      switch item {
      | Acc(val) => input->solve(acc + val, set->Belt.Set.Int.add(index), index + 1)
      | Jmp(val) => input->solve(acc, set->Belt.Set.Int.add(index), index + val)
      | Nop(_) => input->solve(acc, set->Belt.Set.Int.add(index), index + 1)
      }
    })
  }
}

let p1 = input->solve(0, Belt.Set.Int.empty, 0)

switch p1 {
| Infinite(val) => val
| Finite(_) => 0
}->Js.log

let p3 =
  input
  ->mapWithIndex((i, x) => {
    switch x {
    | Acc(_) => None
    | Jmp(val) =>
      input
      ->Vector.set(i, Nop(val))
      ->Belt.Option.map(input => input->solve(0, Belt.Set.Int.empty, 0))
    | Nop(val) =>
      input
      ->Vector.set(i, Jmp(val))
      ->Belt.Option.map(input => input->solve(0, Belt.Set.Int.empty, 0))
    }
  })
  ->Vector.keepMap(Garter.Fn.identity)
  ->Vector.keep(x => {
    switch x {
    | Infinite(_) => false
    | Finite(_) => true
    }
  })
  ->Vector.get(0)

p3
->Belt.Option.mapWithDefault(0, p3 => {
  switch p3 {
  | Infinite(_) => 0
  | Finite(val) => val
  }
})
->Js.log