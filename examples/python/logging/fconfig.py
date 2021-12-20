#!/usr/bin/env python

import logging, logging.config, sys, os, json
from logging.handlers import RotatingFileHandler
from logging import Formatter

log = logging.getLogger(__name__)

def init_logging(cfg_fname='pylogcfg.json'):
    cfgpath = cfg_fname
    if os.getenv('PYLOGCFG', None) is not None:
        cfgpath = os.getenv('PYLOGCFG')

    if not os.path.exists(cfgpath):
        raise FileNotFoundError("Error: {} does not exist!".format(cfgpath))

    with open(cfgpath, 'rt') as infh:
        config = json.load(infh)

    logging.config.dictConfig(config)

def test_logging():
  log.debug("log.debug() is ON")
  log.info("log.info() is ON")
  log.warning("log.warning() is ON")
  log.error("log.error() is ON")
  log.fatal("log.fatal() is ON")
  log.critical("log.critical() is ON")

if __name__ == '__main__':
    log.critical("before init_logging")
    test_logging()
    init_logging()
    log.critical("after init_logging")
    test_logging()
