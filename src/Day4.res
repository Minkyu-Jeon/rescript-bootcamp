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

let fieldsToMap = (x: array<array<string>>): Belt_MapString.t<string> =>
  x->Belt.Array.reduce(Belt.Map.String.empty, addFieldToMap)

let parsePassport = (passportKeyValueMap: Belt_MapString.t<string>, validate): option<passport> => {
  switch passportKeyValueMap->validate {
  | Some(map) => {
      let byr =
        map->Belt.Map.String.getExn("byr")->Belt.Int.fromString->Belt.Option.getWithDefault(0)
      let iyr =
        map->Belt.Map.String.getExn("iyr")->Belt.Int.fromString->Belt.Option.getWithDefault(0)
      let eyr =
        map->Belt.Map.String.getExn("eyr")->Belt.Int.fromString->Belt.Option.getWithDefault(0)

      Some({
        byr: byr,
        iyr: iyr,
        eyr: eyr,
        hgt: map->Belt.Map.String.getExn("hgt"),
        hcl: map->Belt.Map.String.getExn("hcl"),
        ecl: map->Belt.Map.String.getExn("ecl"),
        pid: map->Belt.Map.String.getExn("pid"),
        cid: map->Belt.Map.String.get("cid"),
      })
    }
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

let validatePassportFormat = (x: passport): bool => {
  x.byr >= 1920 &&
  x.byr <= 2002 &&
  x.iyr >= 2010 &&
  x.iyr <= 2020 &&
  x.eyr >= 2020 &&
  x.eyr <= 2030 &&
  switch x.hgt->matchedStrings(%re("/^(\d+)(cm|in)$/")) {
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
  } &&
  %re("/^#[\da-f]{6}$/")->Js.Re.test_(x.hcl) &&
  %re("/^amb|blu|brn|gry|grn|hzl|oth$/")->Js.Re.test_(x.ecl) &&
  %re("/^\d{9}$/")->Js.Re.test_(x.pid)
}

let validateMapFields = (requiredFields: array<string>, map: Belt_MapString.t<string>): option<
  Belt_MapString.t<string>,
> => {
  switch requiredFields->Belt.Array.every(map->Belt.Map.String.has) {
  | true => Some(map)
  | false => None
  }
}
let validateRequiredFields = requiredFields => requiredFields->validateMapFields
let parseWithValidateRequiredFields = (requiredFields, map) =>
  map->parsePassport(validateRequiredFields(requiredFields))

let requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

let p1 =
  input
  ->Belt.Array.map(fieldsToMap)
  ->Belt.Array.keepMap(parseWithValidateRequiredFields(requiredFields))
  ->Belt.Array.length

p1->Js.log

let p2 =
  input
  ->Belt.Array.map(fieldsToMap)
  ->Belt.Array.keepMap(parseWithValidateRequiredFields(requiredFields))
  ->Belt.Array.keep(validatePassportFormat)
  ->Belt.Array.length

p2->Js.log
