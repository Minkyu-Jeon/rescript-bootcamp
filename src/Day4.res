let input = Node.Fs.readFileAsUtf8Sync("input/day4.sample.txt")->Js.String2.split("\n")

let requiredField = list{"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"} // cid is optional

input->Belt.Array.reduce(list{list{}}, (acc, line) => {
    if ( line == "" ) {
        acc->Belt.List.add(list{})
    } else {
        let head = acc->Belt.List.head
        let newAcc = acc->Belt.List.drop(1)
        let destructedHead = switch head {
            | None => list{}
            | Some(value) => value
        }
        switch newAcc {
            | None => list{}
            | Some(newAcc) => {
                let newLine = line->Js.String2.split(" ")->Belt.Array.map(x => {
                    let optionField = x->Js.String2.split(":")->Belt.Array.get(0)
                    switch optionField {
                        | None => ""
                        | Some(value) => value
                    }
                })->Belt.List.fromArray
                newAcc->Belt.List.add(destructedHead->Belt.List.concat(newLine))
            }
        }
    }
})

->Belt.List.reverse->Belt.List.reduce(0, (acc, line) => {
    acc + requiredField->Belt.List.reduce(1, (acc2, rf) => {
        acc2 * (line->Belt.List.has(rf, (a, b) => a == b) ? 1 : 0)
    })
})->Js.log
// ->Belt.List.forEach((x) => {
//     x->Belt.List.map(y => y)->Belt.List.reduce("", (str, item) => str ++ " / " ++ item)->Js.log
//     Js.log("-----")
// })
