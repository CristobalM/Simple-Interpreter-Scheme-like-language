import re
#import pyparsing

class Num:
    def __init__(self, num):
        self.num = num

    def eval(self, env):
        return self.num


class BinaryExpr:
    def __init__(self,  expr1, expr2):
        self.expr1 = expr1
        self.expr2 = expr2


class SumExpr(BinaryExpr):
    def eval(self, env):
        return self.expr1.eval(env) + self.expr2.eval(env)


class DiffExpr(BinaryExpr):
    def eval(self, env):
        return self.expr1.eval(env) - self.expr2.eval(env)


class If0Expr:
    def __init__(self, expr1, expr2, expr3):
        self.expr1 = expr1
        self.expr2 = expr2
        self.expr3 = expr3

    def eval(self, env):
        if self.expr1.eval(env) == 0:
            return self.expr2.eval(env)
        else:
            return self.expr3.eval(env)

class IdExpr:
    def __init__(self, idx):
        self.idx = idx

    def eval(self, env):
        return lookupEnv(self.idx, env)


class FunExpr:
    def __init__(self, idx, body):
        self.idx = idx
        self.body = body

    def eval(self, env):
        return ClosureV(self.idx, self.body, env)


class NotFoundInEnv(Exception):
    def __init__(self, idx):
        self.idx = idx

    def __str__(self):
        return "Not found id: " + self.idx


class EmptyEnv:
    def lookupEnv(self, idx):
        raise NotFoundInEnv(idx)


class Env:
    def __init__(self, idx, value, nextEnv):
        self.idx = idx
        self.value = value
        self.nextEnv = nextEnv

    def lookupEnv(self, idx):
        if self.idx == idx:
            return self.value
        return self.nextEnv.lookupEnv(idx)


def extendEnv(idx, value, env):
    return Env(idx, value, env)


def lookupEnv(idx, env):
    return env.lookupEnv(idx)


class AppExpr:
    def __init__(self, funExpr, argExpr):
        self.funExpr = funExpr
        self.argExpr = argExpr

    def eval(self, env):
        closure = self.funExpr.eval(env)
        idx = closure.idx
        body = closure.body
        fenv = closure.env

        argVal = self.argExpr.eval(env)
        extendedEnv = extendEnv(idx, argVal, fenv)

        return body.eval(extendedEnv)


class ClosureV:
    def __init__(self, idx, body, env):
        self.idx = idx
        self.body = body
        self.env = env

    def __str__(self):
        return "Function"


parenthesesBlock = r"\(.*?\)"
#parenthesesBlock = r"\(([^()]*|(?R))*\)"
anyItem = r"(" + r"[^\s()]+" + r"|" + parenthesesBlock + r")"  # any number or another block of parentheses
atLeastOneWhiteSpace = r"\s+"
anyWhiteSpaces = r"\s*"
startParentheses = r"^" + anyWhiteSpaces + r"\(" + anyWhiteSpaces
endParentheses = anyWhiteSpaces + r"\)" + anyWhiteSpaces + r"$"


def opRegex(which, argsnum):
    condWhiteSpace = ""
    if argsnum >= 1:
        args_str = (anyItem + atLeastOneWhiteSpace) * (argsnum-1) + anyItem
        condWhiteSpace = atLeastOneWhiteSpace
    else:
        args_str = ""

    if which == "":
        condWhiteSpace = ""

    return startParentheses + which + condWhiteSpace + args_str + endParentheses



numRegex = r"(^-?\d+$)"
idRegex = r"(^[^\s]+$)"
sumRegex = opRegex(r"\+", 2)
diffRegex = opRegex(r"\-", 2)
if0Regex = opRegex(r"\bif0\b", 3)
#withRegex = opRegex(r"\bwith\b", 3)
withRegex = startParentheses + r"\bwith\b" +\
            anyWhiteSpaces + r"\(" + anyWhiteSpaces +\
            r"([^\s]+)" + atLeastOneWhiteSpace + anyItem +\
            anyWhiteSpaces + r"\)" + anyWhiteSpaces + anyItem + endParentheses
#funRegex = opRegex(r"\bfun\b", 2)
funRegex = startParentheses + r"\bfun\b" +\
    anyWhiteSpaces + r"\(" + anyWhiteSpaces + r"([^\s]+)" + anyWhiteSpaces + r"\)" +\
    anyWhiteSpaces + anyItem + endParentheses
appRegex = opRegex(r"", 2)

print "appRegex!: '" + appRegex + "'"





class ParsingError(Exception):
    def __init__(self, s):
        self.s = s

    def __str__(self):
        return "Can't parse string: '" + self.s + "'"

def parser(s):
    #print "Parsing string '" + s + "' ..."
    numMatch = re.search(numRegex, s)
    if numMatch:
        #print "numMatch"
        return Num(int(numMatch.group(1)))

    idMatch = re.search(idRegex, s)
    if(idMatch):
        return IdExpr(idMatch.group(1))

    sumMatch = re.search(sumRegex, s)

    if sumMatch:
        #print "sumMatch"
        return SumExpr(parser(sumMatch.group(1)),
                       parser(sumMatch.group(2)))

    diffMatch = re.search(diffRegex, s)

    if diffMatch:
        #print "diffMatch"
        return DiffExpr(parser(diffMatch.group(1)),
                        parser(diffMatch.group(2)))

    if0Match = re.search(if0Regex, s)

    if if0Match:
        #print "if0Match"
        return If0Expr(parser(if0Match.group(1)),
                       parser(if0Match.group(2)),
                       parser(if0Match.group(3)))

    #print "no match"

    withMatch = re.search(withRegex, s)
    if withMatch:
        return AppExpr(FunExpr(withMatch.group(1), parser(withMatch.group(3))),
                       parser(withMatch.group(2)))


    funMatch = re.search(funRegex, s)
    if(funMatch):
        return FunExpr(funMatch.group(1), parser(funMatch.group(2)))

    appMatch = re.search(appRegex, s)
    if(appMatch):
       # print "appMatch"
      #  print "group 1: '"+ appMatch.group(1) + "'"
       # print "group 2: '"+ appMatch.group(2) + "'"
        return AppExpr(parser(appMatch.group(1)), parser(appMatch.group(2)))

    raise ParsingError(s)


def interp(expr, env):
    return expr.eval(env)


def run(s):
    parsed = parser(s)
    env = EmptyEnv()
    interpreted = interp(parsed, env)
    output = str(interpreted)
    return output

"""
print run("(- 3 (+ 5 (+ 0 1000)))")
print run("(   if0   1     1     2     )")
print run("   (with (x 5) (+ x (with (y 3) (+ 10 y))))")
print run("     (       with (      x   (    fun    (     y    ) (    +     123     y   )   ) )  (  x   1      ))   ")
print run("(with (x (fun (y) (+ 123 y)))(x 1))")
print run("     (     with (      x      (fun (x) (+ 123 x))) (x 0))")
#print run("(with (y 3) (+ x y))")
print run("(with (x (fun (x) "
          "(with (y 3) (+ x y) )"
          "))(x 5))")
print run("(fun (x) x)")
"""
print run ("( "
           "( "
           "(fun (x) x ) "
           "(fun (x) (+ x 5)) "
           ") 3 )")

#print run("(with (x 3) x")
print run("(with (x 3)(with (f (fun (y) (+ x y)))(with (x 5) (f 4))))")