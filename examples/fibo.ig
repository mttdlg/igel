#!/usr/bin/env igel

#
# Example: Fibonacci sequence
#

#
# New syntax for declarations/definitions:
#
#   name : <type> = <value>
#
# Depending on the construct, type and value could both be optional.
# Some constructs (like 'const') might require the value.
# Other constructs (like 'var') might require at least one:
#   - if only the value is provided, we can rely on type inference.
#   - if only the type is provided, we attempt to call a default constructor.
#       (or raise an exception if it is not available)
# 
# If the type is not specified, we use type inference -- in this case, the value is mandatory).
#
# In general, ':' will be associated with type specifications, and
# '=' will be associated with value specification.
#
# That is why key/values tables will probably be written as:
#
#   {
#     a = 5
#     b = 6
#   }
#
# using '=' instead of the more Python-like:
#
#   {
#     a : 5
#     b : 6
#   }
#
# To keep things regular/consistent, : will be reserved for specifying types, not values.
#
# Example:
#
# struct Foo {
#   name  : string
#   count : int
# }
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
