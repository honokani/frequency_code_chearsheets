# original modules
from lib.logging.logging_decos import deco_log_common, getlogger

logger = getlogger(__name__)
def deco_log_greet(f): return deco_log_common(logger,f)

class Greet():
    @deco_log_greet
    def __init__(self, n):
        self.name = n
        logger.debug("Greet instance has created.")

    @deco_log_greet
    def hello(self, h):
        logger.debug("hello function of Greet has called.")
        h24 = h % 24
        if (5<h24 and h24<12):
            return "good morning, {}.".format(self.name)
        elif (12<=h24 and h24<18):
            return "good afternoon, {}.".format(self.name)
        elif (18<=h24 and h24<21):
            return "good evening, {}.".format(self.name)
        else:
            return "hi, {}.".format(self.name)

    @deco_log_greet
    def bye(self):
        logger.debug("bye function of Greet has called.")
        self.locate_test()
        return "bye, {}.".format(self.name)

    @deco_log_greet
    def locate_test(self):
        return 0

