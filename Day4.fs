module Day4

let getRange (assignment: string) =
    let splited = assignment.Split('-')
    (splited[0] |> int, splited[1] |> int)

let isContained (b1, e1) (b2, e2) =
    b1 <= b2 && e1 >= e2 || 
    b2 <= b1 && e2 >= e1

let isOverlaped (b1, e1) (b2, e2) =
    e1 >= b2 && e2 >= b1

let campCleanup =
    let lines = Utils.getLines 4
    let mutable count = 0

    for l in lines do
        let pairs = l.Split(',')

        if isOverlaped (getRange pairs[0]) (getRange pairs[1]) then
            count <- count + 1

    count