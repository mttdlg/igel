#!/usr/bin/env igel

#
# Not functional, yet, just a mock-up for planned featurs.
#

const cycles_max: Uint 16

proc monitor_inf { inf: Interface } : XyzDataItem {
    set result [XyzDataItem]
    using result {
        clocked inf.@clock {
            while ([inf.req] != "1") {
                wait cycle
            }
            set .addr [inf.addr]
            set .opc  [inf.opc]
            if (inf.opc == 'WRITE) {
                set .wdata [inf.wdata]
            }
            while ([inf.gnt] != "1") {
                wait cycle -timeout max_cycles -on_timeout { dut_error "Timeout on grant!" }
            }
        }
    }
}
