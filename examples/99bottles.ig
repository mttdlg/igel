#!/usr/bin/env igel

const startingBottles: int 99;

proc nToMaybeWords {n: int} : string {
    if [eq n 0] { return "no more" }
    if [eq n 1] { return "one" }
    return [cat n]  # [cat n] : Trick to convert number -> string
                    # Normally, 'cat' (whose name was inspired by
                    # the corresponding shell command), returns a
                    # string obtained by conCATenating the string
                    # representations of its arguments, no matter
                    # what the arguments' types are.
                    #
                    # According to this rule, if only one
                    # argument is provided, cat returns its
                    # string representation.
}

proc nToCapitalMaybeWords {n: int} : string {
    if [eq n 0] { return "No more" }
    if [eq n 1] { return "One" }
    return [cat n]  # [cat n] : Trick to convert number -> string
}

proc bottlesString {n: int; capitalize } : string {
    var s: string

    if capitalize {
        set s [nToCapitalMaybeWords n]
    } else {
        set s [nToMaybeWords n]
    }
    set s [cat s " bottle"]
    if [ne n 1] {
        set s [cat s "s"]
    }
    return s
}

proc pronoun {n: int} : string {
    if [eq n 1] { return "it" }
    return "one"
}

proc mainBottles {max_n: int} {
    var n: int = max_n
    while [ge n 0] {
        write [bottlesString n true]  " of beer on the wall, "
        print [bottlesString n false] " of beer."

        var n1: int = [sub n 1]
        if [ge n1 0] {
            write "Take " [pronoun n] " down, pass it around, "
            print [bottlesString n1 false] " of beer on the wall!"
            print
        } else {
            write "Go to the store and buy some more, "
            print [bottlesString max_n false] " of beer on the wall!"
        }
        set n n1
    }
}

mainBottles startingBottles
