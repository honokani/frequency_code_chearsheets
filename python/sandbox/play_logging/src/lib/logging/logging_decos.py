# common modules
import os
import logging
import inspect
# log module
from lib.logging.logger_setting import set_logger_conf

set_logger_conf()

def getlogger(tgt):
    return logging.getLogger(tgt)

def location(depth=0):
    curr = inspect.currentframe()
    info = inspect.getouterframes(curr)[depth+1]
    return  os.path.basename(info.filename),info.lineno

def log_at_timing(lgr,f,level=20,timing="undef"):
    # This function is calld 3 time reflections.
    lna, lno = location(3)
    jlno = str(lno).rjust(3,"0")
    jtm, jfn, jfm = timing.ljust(2), f.__name__.ljust(16), f.__module__.ljust(12)
    msg = "{}: {} in {} @ L.{} {}".format(jtm, jfn, jfm, jlno, lna)
    lgr.log(level, msg)

def log_at_start(lgr,f):
    tm = "S"
    lv = logging.INFO
    log_at_timing(lgr, f, lv, tm)

def log_at_end(lgr,f):
    tm = "E"
    lv = logging.ERROR
    log_at_timing(lgr, f, lv, tm)

def deco_log_core(lgr,f):
    def w(*a, **k):
        log_at_start(lgr,f)
        res = f(*a, **k)
        log_at_end(lgr,f)
        return res
    return w

def deco_log_common(logger,f):
    # set level filer
    lv_mode = logging.DEBUG
    logger.setLevel(lv_mode)
    return deco_log_core(logger,f)

