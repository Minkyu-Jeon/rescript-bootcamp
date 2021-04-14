let input = Node.Fs.readFileAsUtf8Sync("input/day4.sample.txt")
->Js.String2.split("\n\n")
->Belt.Array.map(x => x->Js.String2.split("\n"))
->Belt.Array.map(x => {
  x
  ->Belt.Array.map(y => {y->Js.String2.split(" ")})
  ->Belt.Array.concatMany
  ->Belt.Array.map(x => x->Js.String2.split(":"))
})

type passport = {
  byr: int,
  iyr: int,
  eyr: int,
  hgt: string,
  hcl: string,
  ecl: string,
  pid: string,
  cid: option<string>,
}

let addFieldToMap = (map, fieldWithValue) => {
  switch fieldWithValue {
  | [field, value] => map->Belt.Map.String.set(field, value)
  | _ => map
  }
}

let fieldsToMap = x => x->Belt.Array.reduce(Belt.Map.String.empty, addFieldToMap)

let getStrUnsafe = (map, key) => map->Belt.Map.String.getExn(key)
let getIntUnsafe = (map, key) => map->getStrUnsafe(key)->Belt.Int.fromString->Belt.Option.getExn

let parsePassport = (validate, passportKeyValueMap) => {
  switch passportKeyValueMap->validate {
  | Some(map) =>
    Some({
      byr: map->getIntUnsafe("byr"),
      iyr: map->getIntUnsafe("iyr"),
      eyr: map->getIntUnsafe("eyr"),
      hgt: map->getStrUnsafe("hgt"),
      hcl: map->getStrUnsafe("hcl"),
      ecl: map->getStrUnsafe("ecl"),
      pid: map->getStrUnsafe("pid"),
      cid: map->Belt.Map.String.get("cid"),
    })
  | None => None
  }
}

let matchedStrings = (str, re) => {
  re
  ->Js.Re.exec_(str)
  ->Belt.Option.mapWithDefault([Js.Nullable.return("")], x => x->Js.Re.captures)
  ->Belt.Array.sliceToEnd(1)
  ->Belt.Array.map(Js.Nullable.toOption)
}

// Passport format check
let toSome = x => Some(x)
let parsePassportFormat = x => {
  let byr =
    x
    ->Belt.Map.String.get("byr")
    ->Belt.Option.flatMap(x => x->Belt.Int.fromString)
    ->Belt.Option.map(x => x >= 1920 && x <= 2002)
  let iyr =
    x
    ->Belt.Map.String.get("iyr")
    ->Belt.Option.flatMap(x => x->Belt.Int.fromString)
    ->Belt.Option.map(x => x >= 2010 && x <= 2020)
  let eyr =
    x
    ->Belt.Map.String.get("eyr")
    ->Belt.Option.flatMap(x => x->Belt.Int.fromString)
    ->Belt.Option.map(x => x >= 2020 && x <= 2030)
  let hgt =
    x
    ->Belt.Map.String.get("hgt")
    ->Belt.Option.map(x => {
      switch x->matchedStrings(%re("/^(\d+)(cm|in)$/")) {
      | [Some(height), Some(heightUnit)] => {
          let heightInt = height->Belt.Int.fromString->Belt.Option.getWithDefault(0)
          if heightUnit == "cm" {
            heightInt >= 150 && heightInt <= 193
          } else if heightUnit == "in" {
            heightInt >= 59 && heightInt <= 76
          } else {
            false
          }
        }
      | _ => false
      }
    })
  let hcl =
    x->Belt.Map.String.get("hcl")->Belt.Option.map(x => %re("/^#[\da-f]{6}$/")->Js.Re.test_(x))
  let ecl =
    x
    ->Belt.Map.String.get("ecl")
    ->Belt.Option.map(x => %re("/^amb|blu|brn|gry|grn|hzl|oth$/")->Js.Re.test_(x))
  let pid = x->Belt.Map.String.get("pid")->Belt.Option.map(x => %re("/^\d{9}$/")->Js.Re.test_(x))

  let result =
    [byr, iyr, eyr, hgt, hcl, ecl, pid]
    ->Belt.Array.keepMap(Garter.Fn.identity)
    ->Belt.Array.every(Garter.Fn.identity)

  result ? Some(x) : None
}

let validateMapFields = (requiredFields, map) => {
  switch requiredFields->Belt.Array.every(map->Belt.Map.String.has) {
  | true => Some(map)
  | false => None
  }
}
let validateRequiredFields = requiredFields => requiredFields->validateMapFields

let requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

let solve = (input, parsePassportFn) => {
  input
  ->Belt.Array.map(fieldsToMap)
  ->Belt.Array.keepMap(validateRequiredFields(requiredFields))
  ->Belt.Array.keepMap(parsePassport(parsePassportFn))
  ->Belt.Array.length
}

let p1 = input->solve(toSome)

p1->Js.log

let p2 = input->solve(parsePassportFormat)

p2->Js.log
