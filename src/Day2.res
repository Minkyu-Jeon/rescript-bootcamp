let input =
  Node.Fs.readFileAsUtf8Sync("input/day2.sample.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(x => x->Js.String2.split(" "))

type password = {
  rule: (int, int),
  char: string,
  password: array<string>,
}

let parseRule = rule => {
  rule
  ->Js.String2.split("-")
  ->Belt.Array.map(x => x->Belt.Int.fromString->Belt.Option.getWithDefault(0))
}

let parsePassword = x => {
  switch x {
  | [rule, char, password] => {
      let parsedRule = switch parseRule(rule) {
      | [min, max] => (min, max)
      | _ => "invalid Format: rule"->Js.Exn.raiseError
      }
      let passwordChars = password->Js.String2.split("")

      {rule: parsedRule, char: char, password: passwordChars}
    }
  | _ => "invalid Format: line"->Js.Exn.raiseError
  }
}

let selectCharsInCriteria = (pwInfo: password) => {
  let (min, max) = pwInfo.rule
  let passwordCharsInCriteria =
    [min, max]->Belt.Array.map(idx => pwInfo.password->Belt.Array.getExn(idx - 1))
  {...pwInfo, password: passwordCharsInCriteria, rule: (1, 1)}
}

let toMatchedCharCountAndRuleTuple = (pwInfo: password) => (
  pwInfo.password->Belt.Array.keep(x => `${x}:` == pwInfo.char)->Belt.Array.length,
  pwInfo.rule,
)
let validateMatchedCharCount = ((matchedCharCount, (min, max))) =>
  matchedCharCount >= min && matchedCharCount <= max

let p1 =
  input
  ->Belt.Array.map(parsePassword)
  ->Belt.Array.map(toMatchedCharCountAndRuleTuple)
  ->Belt.Array.keep(validateMatchedCharCount)
  ->Belt.Array.length
p1->Js.log

let p2 =
  input
  ->Belt.Array.map(parsePassword)
  ->Belt.Array.map(selectCharsInCriteria)
  ->Belt.Array.map(toMatchedCharCountAndRuleTuple)
  ->Belt.Array.keep(validateMatchedCharCount)
  ->Belt.Array.length
p2->Js.log
