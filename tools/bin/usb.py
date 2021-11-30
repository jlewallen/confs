#!/usr/bin/python3

import capablerobot_usbhub

print(capablerobot_usbhub)
print(dir(capablerobot_usbhub))

hub = capablerobot_usbhub.USBHub()

ports = [1, 2, 3, 4]
for port in ports:
    logger.info("enable port %d" % port)
    hub.power.enable([port])
    time.sleep(0.5)
    logger.info("disable port %d" % port)
    hub.power.disable([port])
    time.sleep(0.5)
