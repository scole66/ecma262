


class Lexer(object):
    def __init__(self, stream):
        super().__init__()
        self.stream = stream
        self.linenum = 0
