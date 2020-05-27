#!/usr/bin/env igel

var i: int 0
# set i 0
while [lt i 10] {
    print "\"i\" is " i
    set i [add i 1]
}
print "Successfully exited while loop"
# print [parse [a b c]]

