from pymeta.runtime import ParseError

class ParsingMixin(object):

    def assertParses(self, data, grammar, rule):
        try:
            return grammar(data).apply(rule)[0]
        except ParseError, pe:
            assert False, pe.formatError(data)
