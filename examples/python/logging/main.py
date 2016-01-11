#!/usr/bin/env python

import logging, sys
from logging.handlers import RotatingFileHandler
from logging import Formatter


# def init_logging():
#     ensure_dir_for_file(app.config['LOGGING']['filename'])
#     logging_handler = RotatingFileHandler(app.config['LOGGING']['filename'], maxBytes=1024*1024*10, backupCount=5)
#     logging_handler.setLevel(logging.INFO)
#     if app.config['DEBUG']:
#         logging_handler.setLevel(logging.DEBUG)
#         app.logger.addHandler(logging_handler)
#         logging_handler.setFormatter(Formatter(

# see: http://victorlin.me/posts/2012/08/26/good-logging-practice-in-python
#  look for the section "Do not get logger at the module level unless disable_existing_loggers is False"
_logger = None
def logger():
    global _logger
    if _logger is None:
      _logger = logging.getLogger(__name__)
    return _logger


def log_stuffs():
    logger().debug("this is a debug statement")
    logger().info("this is an info statement")
    logger().warning("this is a warning statement")
    logger().error("this is an error statement")
    logger().exception("this is an exception statement")


print("Logging at: {}".format(logging.getLevelName(logger().getEffectiveLevel())))
log_stuffs()
logger().setLevel(logging.DEBUG)
print("Logging at: {}".format(logging.getLevelName(logger().getEffectiveLevel())))
log_stuffs()

def throws_exception():
  raise Exception("yikes!")

try:
  throws_exception()
except:
  e = sys.exc_info()[0]
  logger().error('throws_exception failed!: {}'.format(e), exc_info=True)

