class Greet():
    def __init__(self, n):
        self.name = n

    def hello(self, h):
        h24 = h % 24
        if (5<h24 and h24<12):
            return "good morning, {}.".format(self.name)
        elif (12<=h24 and h24<18):
            return "good afternoon, {}.".format(self.name)
        elif (18<=h24 and h24<21):
            return "good evening, {}.".format(self.name)
        else:
            return "hi, {}.".format(self.name)
    def bye(self):

        return "bye, {}.".format(self.name)

