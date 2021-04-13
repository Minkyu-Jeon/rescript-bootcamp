let input =
  Node.Fs.readFileAsUtf8Sync("input/day2.sample.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(x => x->Js.String2.split(" "))

type rule = (int, int)

type password = {
  rule: rule,
  char: string,
  password: string, // <- 그냥 string
}

// raiseError는 아껴쓰기! option (혹은 result)로 파싱하는 것을 권장합니다.
// https://rescript-lang.org/docs/manual/latest/api/belt/result
let parseRule = (rule): option<rule> => {
  let f = rule =>
    rule
    ->Js.String2.split("-")
    ->Belt.Array.map(x => x->Belt.Int.fromString->Belt.Option.getWithDefault(0))

  switch f(rule) {
  | [min, max] => Some((min, max))
  | _ => None
  }
}

let parsePassword = (x): option<password> => {
  switch x {
  | [rule, char, password] =>
    switch parseRule(rule) {
    | Some(rule) =>
      Some({
        rule: rule,
        char: char->Js.String2.get(0),
        password: password,
      })
    | None => None
    }
  | _ => None
  }
}

let isIncludeCharsInMinMax = (x: password): bool => {
  let (min, max) = x.rule
  let matchedCharCount =
    x.password
    ->Js.String2.split("")
    ->Belt.Array.reduce(0, (acc, char) => {acc + (char == x.char ? 1 : 0)})

  matchedCharCount >= min && matchedCharCount <= max
}

let isIncludeCharOnce = (x: password): bool => {
  let (start, end) = x.rule
  let matchedCharCount = [start, end]->Belt.Array.keep(idx => x.password->Js.String2.get(idx - 1) == x.char)->Belt.Array.size

  matchedCharCount == 1
}

let solve = (input, validateFn) => {
  input
  ->Belt.Array.keepMap(parsePassword)
  ->Belt.Array.keep(validateFn)
  ->Belt.Array.length
}

let p1 = input->solve(isIncludeCharsInMinMax)
p1->Js.log

let p2 = input->solve(isIncludeCharOnce)
p2->Js.log
