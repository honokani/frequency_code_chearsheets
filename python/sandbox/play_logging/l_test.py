import logging
import l_test_greet as G

logging.basicConfig(
    level=logging.DEBUG,
    format='%(asctime)s %(levelname)-8s %(module)-18s %(funcName)-10s %(lineno)4s: %(message)s'
)

def deco_s_e(f):
    def decorate(*x,**y):
        n = f.__name__
        logging.info("{0} has started.".format(n))
        r = f(*x,**y)
        logging.info("{0} has ended.".format(n))
        return r
    return decorate


@deco_s_e
def main():
    greeter = G.Greet("tester")
    logging.debug("generated")
    print(greeter.hello(6))
    logging.debug("called: hello")
    print(greeter.bye())
    logging.debug("called: bye")

if (__name__=="__main__"):
    main()

