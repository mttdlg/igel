#!/usr/bin/env igel

proc pick {f_cmp : Fn; a: Int; b: Int} {
    if [f_cmp a b] {
        return a
    } else {
        return b
    }
}

proc min2 {a: Int; b: Int} { return [pick le a b] }
proc max2 {a: Int; b: Int} { return [pick gt a b] }

proc gcd_ordered {a: Int; b: Int} {

    var remainder: Int [mod a b]
    print "this step: " a " " b " -> " remainder

    if [eq remainder 0] {
        return b
    } else {
        return [gcd_ordered b remainder]
    }
}

proc gcd {a: Int; b: Int} {
    return [gcd_ordered [max2 a b]
                        [min2 a b]]
}

print [gcd 18 48]

