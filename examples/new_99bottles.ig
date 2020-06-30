#!/usr/bin/env igel

const startingBottles: int 99;

proc bottlesString {n: int; capitalize: bool} : string {
    var s: string

    if [gt n 1] {
        set s [cat n] # [cat n] -> trick to convert number to string
    } elif [eq n 1] {
        if capitalize { set s "No more" } else { set s "no more" }
    } elif [eq n 0] {
        if capitalize { set s "One" } else { set s "one" }
    } else { 
        set s "???"
    }

    if false {
        #
        # Memo for syntax:
        #
        cond {
            [gt n 1] { set s [cat n] } # cat n -> converts number to string 
            [eq n 1] { if capitalize { set s "No more" } else { set s "no more" } }
            [eq n 0] { if capitalize { set s "One" } else { set s "one" } }
            true     { set s "???" }
        }
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
    # state n [Bit 4 0] reset: 0
    # state n [Mvl 4 0] -reset "00000"
    # state n [Mvl 4 0] -reset 0
    # state n [Mvl 4 0] -reset "XXXXX"
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
