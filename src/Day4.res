// let input = Node.Fs.readFileAsUtf8Sync("input/day4.sample.txt")->Js.String2.split("\n")

// let requiredField = list{"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"} // cid is optional
// type passportField = 
//     | None
//     | Byr(string)
//     | Iyr(string)
//     | Eyr(string)
//     | Hgt(string)
//     | Hcl(string)
//     | Ecl(string)
//     | Pid(string)
//     | Cid(string)

// input->Belt.Array.reduce(list{list{}}, (acc, line) => {
//     if ( line == "" ) {
//         acc->Belt.List.add(list{})
//     } else {
//         let head = acc->Belt.List.head
//         let newAcc = acc->Belt.List.drop(1)
//         let destructedHead = switch head {
//             | None => list{}
//             | Some(value) => value
//         }
//         switch newAcc {
//             | None => list{}
//             | Some(newAcc) => {
//                 let newLine = line->Js.String2.split(" ")->Belt.Array.map(x => {
//                     let splittedString = x->Js.String2.split(":")
//                     let optionField = splittedString->Belt.Array.get(0)
//                     let optionValue = splittedString->Belt.Array.get(1)
                
//                     let field = switch optionField {
//                         | None => ""
//                         | Some(field) => field
//                     }
//                     let value = switch optionValue {
//                         | None => ""
//                         | Some(value) => value
//                     }

//                     if ( field == "byr" ) {
//                         Byr(value)
//                     } else if ( field == "iyr" ) {
//                         Iyr(value)
//                     } else if ( field == "eyr" ) {
//                         Eyr(value)
//                     }else if ( field == "hgt" ) {
//                         Hgt(value)
//                     } else if ( field == "hcl" ) {
//                         Hcl(value)
//                     } else if ( field == "ecl" ) {
//                         Ecl(value)
//                     } else if ( field == "pid" ) {
//                         Pid(value)
//                     } else if ( field == "cid" ) {
//                         Cid(value)
//                     } else {
//                         None
//                     }

//                 })->Belt.List.fromArray
//                 newAcc->Belt.List.add(destructedHead->Belt.List.concat(newLine))
//             }
//         }
//     }
// })

// ->Belt.List.reverse->Belt.List.reduce(0, (acc, line) => {
//     acc + line->Belt.List.reduce(1, (acc2, field) => {
//         switch field {
//             | None => acc2 * 0
//             | Byr(value) => {
//                 let optionValue = value->Belt.Int.fromString
//                 let destructedValue = switch optionValue {
//                     | None => 0
//                     | Some(v) => v
//                 }
//                 acc2 * if ( destructedValue >= 1920 && destructedValue <= 2002 ) {
//                     1
//                 } else {
//                     0
//                 }
//             }
//             | Iyr(value) => {
//                 let optionValue = value->Belt.Int.fromString
//                 let destructedValue = switch optionValue {
//                     | None => 0
//                     | Some(v) => v
//                 }
//                 acc2 * if ( destructedValue >= 2010 && destructedValue <= 2020 ) {
//                     1
//                 } else {
//                     0
//                 }
//             }
//             | Eyr(value) => {
//                 let optionValue = value->Belt.Int.fromString
//                 let destructedValue = switch optionValue {
//                     | None => 0
//                     | Some(v) => v
//                 }
//                 acc2 * if ( destructedValue >= 2020 && destructedValue <= 2030 ) {
//                     1
//                 } else {
//                     0
//                 }
//             }
//             | Hgt(value) => {
//                 let height = if ( value->Js.String2.includes("cm") ) {
//                     let re = %re("/\d+/g")
//                     let h = switch re->Js.Re.exec_(value) {
//                         | None => Some("")
//                         | Some(result) => Js.Nullable.toOption(Js.Re.captures(result)[1])
//                     }
//                     h->Js.log
//                     ""
//                 } else if ( value->Js.String2.includes("in") ) {
//                     ""
//                 } else {
//                     ""
//                 }
//                 acc2 * 1
//             }
//             | Hcl(value) => {
//                 acc2 * 1
//             }
//             | Ecl(value) => {
//                 acc2 * 1
//             }
//             | Pid(value) => {
//                 acc2 * 1
//             }
//             | Cid(value) => {
//                 acc2 * 1
//             }
//         }
//     })
// })->Js.log
// // ->Belt.List.forEach((x) => {
// //     x->Belt.List.map(y => y)->Belt.List.reduce("", (str, item) => str ++ " / " ++ item)->Js.log
// //     Js.log("-----")
// // })
