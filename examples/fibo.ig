#!/usr/bin/env igel

#
# New syntax for declarations/definitions:
#
# name : Type = value # ": Type" and "= value" could be optional (type inference, mandatory value).
#

proc fibo {a: Int = 0 ; b: Int = 1} {
    var c: Int = [add a b]

    print [hex a]
    print [hex b]

    while [le c 65535] {
        print [bin c 16] " -> " [hex c 4]
        set a b
        set b c
        set c [add a b]
    }

    print "---> "
    print [hex c]
    print [bin c]
}

fibo 0 1
