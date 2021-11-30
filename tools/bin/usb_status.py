import logging
import time
import capablerobot_usbhub 

logging.basicConfig(format='%(levelname)-8s %(message)s')
logger = logging.getLogger('usb-hub')
logger.setLevel(logging.DEBUG)

hub = capablerobot_usbhub.USBHub()
hub.i2c.enable()

logger.info("Port Connections : {}".format(hub.connections()))
logger.info("Port Speeds      : {}".format(hub.speeds()))

logger.info("Port Currents (mA)")
logger.info("     Measured : " + " ".join([("%.2f" % v).rjust(7) for v in hub.power.measurements()]))
logger.info("     Limit    : {}".format(hub.power.limits()))
logger.info("     Alerts   : {}".format(" ".join(hub.power.alerts())))
logger.info("     State    : {}".format(hub.power.state()))


port = 1

hub.power.disable([port])

time.sleep(1)

hub.power.enable([port])
