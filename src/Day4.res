let input =
  Node.Fs.readFileAsUtf8Sync("input/day4.sample.txt")
  ->Js.String2.split("\n\n")
  ->Belt.Array.map(x => x->Js.String2.split("\n"))

type passport = {
  byr: option<int>,
  iyr: option<int>,
  eyr: option<int>,
  hgt: option<string>,
  hcl: option<string>,
  ecl: option<string>,
  pid: option<string>,
  cid: option<string>,
}

let parseLine = x => {
  x
  ->Belt.Array.map(y => {
    y->Js.String2.split(" ")
  })
  ->Belt.Array.concatMany
}

let toEmptyRecordAndArray = x => (
  {byr: None, iyr: None, eyr: None, hgt: None, hcl: None, ecl: None, pid: None, cid: None},
  x,
)

let matchedStrings = (str, re) => {
  re
  ->Js.Re.exec_(str)
  ->Belt.Option.mapWithDefault([Js.Nullable.return("")], x => x->Js.Re.captures)
  ->Belt.Array.sliceToEnd(1)
  ->Belt.Array.map(Js.Nullable.toOption)
}

let fillRecordField = (acc, x) => {
  switch x->Js.String2.split(":") {
  | [field, value] =>
    if field == "byr" {
      {...acc, byr: value->Belt.Int.fromString}
    } else if field == "iyr" {
      {...acc, iyr: value->Belt.Int.fromString}
    } else if field == "eyr" {
      {...acc, eyr: value->Belt.Int.fromString}
    } else if field == "hgt" {
      {...acc, hgt: Some(value)}
    } else if field == "hcl" {
      {...acc, hcl: Some(value)}
    } else if field == "ecl" {
      {...acc, ecl: Some(value)}
    } else if field == "pid" {
      {...acc, pid: Some(value)}
    } else if field == "cid" {
      {...acc, cid: Some(value)}
    } else {
      `field name '${field}' is not allowed`->Js.Exn.raiseError
    }
  | _ => "Invalid Format"->Js.Exn.raiseError
  }
}

let fillRecord = ((passport, x)) => {
  x->Belt.Array.reduce(passport, fillRecordField)
}

let validateAllSome = x => {
  x.byr->Belt.Option.isSome &&
  x.iyr->Belt.Option.isSome &&
  x.eyr->Belt.Option.isSome &&
  x.hgt->Belt.Option.isSome &&
  x.hcl->Belt.Option.isSome &&
  x.ecl->Belt.Option.isSome &&
  x.pid->Belt.Option.isSome
}

let validatePassportFormat = x => {
  x.byr->Belt.Option.mapWithDefault(false, x => x >= 1920 && x <= 2002) &&
  x.iyr->Belt.Option.mapWithDefault(false, x => x >= 2010 && x <= 2020) &&
  x.eyr->Belt.Option.mapWithDefault(false, x => x >= 2020 && x <= 2030) &&
  x.hgt->Belt.Option.mapWithDefault(false, x => {
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
  }) &&
  x.hcl->Belt.Option.mapWithDefault(false, x => %re("/^#[\da-f]{6}$/")->Js.Re.test_(x)) &&
  x.ecl->Belt.Option.mapWithDefault(false, x =>
    %re("/^amb|blu|brn|gry|grn|hzl|oth$/")->Js.Re.test_(x)
  ) &&
  x.pid->Belt.Option.mapWithDefault(false, x => %re("/^\d{9}$/")->Js.Re.test_(x))
}

let solve = (input, validate) => {
  input
  ->Belt.Array.map(parseLine)
  ->Belt.Array.map(toEmptyRecordAndArray)
  ->Belt.Array.map(fillRecord)
  ->Belt.Array.keep(validate)
  ->Belt.Array.size
}

let p1 = input->solve(validateAllSome)

p1->Js.log

let p2 = input->solve(validatePassportFormat)

p2->Js.log
