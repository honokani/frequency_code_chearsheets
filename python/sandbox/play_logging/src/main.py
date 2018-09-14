# log module
from lib.logging.logging_decos import deco_log_common, getlogger
# original modules
from lib.greet import Greet

logger = getlogger(__name__)
def deco_log_main(f): return deco_log_common(logger,f)

@deco_log_main
def main():
    greeter = Greet("tester")
    logger.debug("call hello 3 times")
    print(greeter.hello(6))
    print(greeter.hello(20))
    print(greeter.bye())

if(__name__=="__main__"):
    main()

