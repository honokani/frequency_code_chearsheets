import logging.config

def set_logger_conf():
    logging.config.dictConfig(conf)

tgt = "logs/log.txt"
conf = {
    "version": 1,
    "formatters": {
        "cfileFormat": {
            "format": "%(asctime)s %(levelname)-5s | %(message)s"
        },
        "cconsoleFormat": {
            "format": "%(asctime)s %(levelname)-5s | %(message)s"
        },
    },
    "handlers": {
        "cfileHandler": {
            "class": "logging.FileHandler",
            "filename": tgt,
            "formatter": "cfileFormat",
            "level": logging.WARNING,
        },
        "cconsoleHandler": {
            "class": "logging.StreamHandler",
            "formatter": "cconsoleFormat",
            "level": logging.DEBUG,
        },
    },

    "root": {
        "handlers": ["cfileHandler","cconsoleHandler"],
        "level": logging.WARNING,
    },
    "loggers": {
        "consoleLogging": {
            "handlers": ["cconsoleHandler"],
            "level": logging.DEBUG,
            'propagate': 0,
        },
    },
}

