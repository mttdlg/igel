#!/usr/bin/env igel

proc pick {f_cmp : [function {int; int} : int]; a: int; b: int} {
    if [f_cmp a b] {
        return a
    } else {
        return b
    }
}

proc min2 {a: int; b: int} { return [pick le a b] }
proc max2 {a: int; b: int} { return [pick gt a b] }

proc gcd_ordered {a: int; b: int} {

    var remainder: int [mod a b]
    print "this step: " a " " b " -> " remainder

    if [eq remainder 0] {
        return b
    } else {
        return [gcd_ordered b remainder]
    }
}

proc gcd {a: int; b: int} {
    return [gcd_ordered [max2 a b]
                        [min2 a b]]
}

print [gcd 18 48]

